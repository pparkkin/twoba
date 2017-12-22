{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.STM.TVar ( TVar
                                   , newTVar
                                   , modifyTVar
                                   , swapTVar
                                   )
import Control.Monad
import Control.Monad.STM ( STM
                         , atomically
                         )
import Data.ByteString ( ByteString )
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
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS

import Game
import Message
import Serializable
import Types

-- http://chimera.labs.oreilly.com/books/1230000000929/ch07.html
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

display :: WS.Connection -> World -> IO ()
display conn world = WS.sendTextData conn (serialize (ServerState world))

processInput :: EventPipe -> World -> IO World
processInput p w = do
  msgs <- takeEvents p
  return $ foldr input w msgs

gameLoop :: World -> Second -> Second -> WS.Connection -> EventPipe -> IO ()
gameLoop world beginTime dt conn input = do
  world' <- processInput input world
  let world'' = update dt world'
  display conn world''

  endTime <- getTimeInSeconds
  let dt' = endTime - beginTime
  when (dt' < frameTime) $ do
    let sleepTime = floor ((frameTime - dt') * (10 ** 6))
    threadDelay sleepTime
  gameLoop world'' endTime dt conn input

getTimeInSeconds :: IO Double
getTimeInSeconds = do
  t <- getCPUTime
  return ((fromIntegral t) / 10^12)

-- Fixed 3 FPS
frameTime = 1.0 / 3.0

newGame :: GameParams -> IO (WS.Connection -> EventPipe -> IO ())
newGame params = do
  time <- getTimeInSeconds
  seed <- getStdGen
  return $ gameLoop (newWorld (Params params) seed :: World) time frameTime

runEventPipe :: Producer -> Consumer -> IO ()
runEventPipe prod cons = do
  p <- newEventPipe
  forkIO (cons p)
  prod p

main :: IO ()
main = do
  putStrLn "Hello, World!"
  app <- application
  Warp.runSettings (Warp.setPort 3000 Warp.defaultSettings)
    $ WaiWS.websocketsOr WS.defaultConnectionOptions wsapp app

application = scottyApp $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "front")
  get "/" $ file "front/index.html"

wsapp :: WS.ServerApp
wsapp pending = do
  conn <- WS.acceptRequest pending
  putStrLn "New connection."
  WS.forkPingThread conn 30
  putStrLn "Waiting for client hello."
  msg <- WS.receiveData conn :: IO ByteString
  putStrLn "Got client hello. Sending server hello."
  let params = GameParams (20, 20)
  WS.sendTextData conn (serialize (ServerHello params))
  game <- newGame params
  putStrLn "Starting new game."
  runEventPipe (echoProd conn) (game conn)

echoProd :: WS.Connection -> EventPipe -> IO ()
echoProd conn p = forever $ do
  msg <- WS.receiveData conn
  pushEvent p msg
