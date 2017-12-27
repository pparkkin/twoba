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
import Control.Monad.STM ( STM
                         , atomically
                         )
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

import Game
import Message
import Serializable
import Types

data GameContext = GameContext
  { params :: GameParams
  , players :: PlayerMap
  , eventpipe :: EventPipe
  , world :: World
  }

data PlayerMap = PlayerMap (TVar (M.Map T.Text WS.Connection))

newPlayerMap :: IO PlayerMap
newPlayerMap = atomically $ do
  m <- newTVar M.empty
  return $ PlayerMap m

addPlayerConnection :: PlayerMap -> T.Text -> WS.Connection -> IO ()
addPlayerConnection (PlayerMap ps) n c = atomically $ do
  m <- readTVar ps
  when (M.size m < 2) $ do
    writeTVar ps (M.insert n c m)

mapPlayerConnections_ :: (WS.Connection -> IO ()) -> PlayerMap -> IO ()
mapPlayerConnections_ a (PlayerMap ps) = do
  m <- atomically $ readTVar ps
  mapM_ a (M.elems m)

playerCount :: PlayerMap -> IO Int
playerCount (PlayerMap ps) = atomically $ do
  m <- readTVar ps
  return (M.size m)

data EventPipe = EventPipe (TVar [Event])
type Event = ByteString

newEventPipe :: IO EventPipe
newEventPipe = atomically $ do
  m <- newTVar []
  return $ EventPipe m

pushEvent :: EventPipe -> Event -> IO ()
pushEvent (EventPipe v) e = atomically $ modifyTVar v (\l -> e : l)

takeEvents :: EventPipe -> IO [Event]
takeEvents (EventPipe v) = atomically $ swapTVar v []

type Consumer = (EventPipe -> IO ())
type Producer = (EventPipe -> IO ())

display :: PlayerMap -> World -> IO ()
display ps world = do
  mapPlayerConnections_ (\conn -> WS.sendTextData conn (serialize (ServerState world))) ps

processInput :: EventPipe -> World -> IO World
processInput p w = do
  msgs <- takeEvents p
  return $ foldr input w msgs

gameLoop :: World -> Second -> Second -> PlayerMap -> EventPipe -> IO ()
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
  return ((fromIntegral t) / 10^12)

-- Fixed 3 FPS
frameTime = 1.0 / 3.0

newGame :: GameParams -> IO GameContext
newGame params = do
  e <- newEventPipe
  m <- newPlayerMap
  seed <- getStdGen
  let w = newWorld (Params params) seed
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
  count <- playerCount ps
  when (count < 2) $ do
    conn <- WS.acceptRequest pending
    putStrLn "New connection."
    WS.forkPingThread conn 30
    putStrLn "Waiting for client hello."
    msg <- WS.receiveData conn :: IO ByteString
    putStrLn $ "Got client hello (" ++ (show msg) ++ "). Sending server hello."
    WS.sendTextData conn (serialize (ServerHello params))
    addPlayerConnection ps (decodeUtf8 msg) conn
    eventListener conn e

eventListener :: WS.Connection -> EventPipe -> IO ()
eventListener conn p = forever $ do
  msg <- WS.receiveData conn
  pushEvent p msg
