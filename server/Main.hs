{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent ( forkIO
                          , threadDelay
                          )
import Control.Concurrent.STM.TVar ( TVar
                                   , newTVar
                                   , modifyTVar
                                   , swapTVar
                                   , readTVar
                                   , writeTVar
                                   )
import Control.Monad
import Data.ByteString ( ByteString )
import Data.Text.Encoding ( decodeUtf8 )
import Network.Wai ( Application )
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static ( staticPolicy
                                     , noDots
                                     , addBase
                                     , (>->)
                                     )
import System.CPUTime
import System.IO
import System.Random ( getStdGen )
import Text.Printf
import Web.Scotty

import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS

import EventPipe
import ConnectionMap
import Game
import Message
import Serializable
import Types

data GameContext = GameContext
  { params :: GameParams
  , players :: ConnectionMap PlayerName
  , eventpipe :: EventPipe Event
  , world :: World
  }

type Event = (PlayerName, ByteString)

displayForPlayer :: World -> PlayerName -> WS.Connection -> IO ()
displayForPlayer world name conn =
  WS.sendTextData conn (output name world)

display :: ConnectionMap PlayerName -> World -> IO ()
display ps world =
  mapConnections_ (displayForPlayer world) ps

processInput :: EventPipe Event -> World -> IO World
processInput p w = do
  msgs <- takeEvents p
  return $ foldr (\(n, msg) w' -> input n msg w') w msgs

gameLoop :: World -> Second -> Second -> ConnectionMap PlayerName -> EventPipe Event -> IO ()
gameLoop world beginTime dt conns input = do
  world' <- processInput input world
  let world'' = update dt world'
  display conns world''

  endTime <- getTimeInSeconds
  let dt' = endTime - beginTime
  when (dt' < frameTime) $ do
    let sleepTime = floor ((frameTime - dt') * (10 ** 6))
    threadDelay sleepTime
  gameLoop world'' endTime dt conns input

getTimeInSeconds :: IO Double
getTimeInSeconds = do
  t <- getCPUTime
  return (fromIntegral t / 10^12)

-- Fixed 3 FPS
frameTime = 1.0 / 3.0

newGame :: GameParams -> IO GameContext
newGame params = do
  e <- newEventPipe
  m <- newConnectionMap
  seed <- getStdGen
  let w = newWorld params seed
  return (GameContext params m e w)

runGame :: GameContext -> IO ()
runGame g@(GameContext params ps e w) = do
  time <- getTimeInSeconds
  gameLoop w time frameTime ps e

main :: IO ()
main = do
  putStrLn "Hello, World!"
  app <- front
  putStrLn "Starting new game."
  g <- newGame (GameParams (20, 20))
  forkIO (runGame g)
  Warp.runSettings (Warp.setPort 3000 Warp.defaultSettings)
    $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsapp g) app

front :: IO Application
front = scottyApp $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "front")
  get "/" $ file "front/index.html"

wsapp :: GameContext -> WS.ServerApp
wsapp g@(GameContext params ps e w) pending = do
  putStrLn "New pending request."
  -- FIXME: Checking count and adding connection should be atomic
  count <- connectionCount ps
  when (count < 2) $ do
    conn <- WS.acceptRequest pending
    putStrLn "New connection."
    WS.forkPingThread conn 30
    putStrLn "Waiting for client hello."
    msg <- WS.receiveData conn :: IO ByteString
    let name = decodeUtf8 msg
    -- FIXME: Needs checking for name collisions
    putStrLn $ "Got client hello (" ++ show msg ++ "). Sending server hello."
    WS.sendTextData conn (serialize (ServerHello (worldInit w)))
    addConnection ps name conn
    pushEvent e (name, serialize AddPlayer)
    eventListener name conn e

eventListener :: PlayerName -> WS.Connection -> EventPipe Event -> IO ()
eventListener n conn p = forever $ do
  msg <- WS.receiveData conn
  pushEvent p (n, msg)
