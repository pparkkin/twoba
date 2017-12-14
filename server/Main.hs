{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS

import Control.Concurrent
import Control.Monad
import System.CPUTime
import System.IO
import Text.Printf

import Game
import Serializable

-- http://chimera.labs.oreilly.com/books/1230000000929/ch07.html
data EventPipe = EventPipe (MVar Event)
data Event = Message T.Text | Stop (MVar ())

type Consumer = (EventPipe -> IO ())
type Producer = (EventPipe -> IO ())

display :: Serializable a => WS.Connection -> a -> IO ()
display conn world = WS.sendTextData conn (serialize world)

gameLoop :: (Serializable a, Game a) => a -> Second -> Second -> WS.Connection -> EventPipe -> IO ()
gameLoop world beginTime dt conn input = do
  let world' = update world dt
  display conn world'

  endTime <- getTimeInSeconds
  let dt' = endTime - beginTime
  when (dt' < frameTime) $ do
    let sleepTime = floor ((frameTime - dt') * (10 ** 6))
    threadDelay sleepTime
  gameLoop world' endTime dt conn input

getTimeInSeconds :: IO Double
getTimeInSeconds = do
  t <- getCPUTime
  return ((fromIntegral t) / 10^12)

-- Fixed 3 FPS
frameTime = 1.0 / 3.0

newGame :: IO (WS.Connection -> EventPipe -> IO ())
newGame = do
  time <- getTimeInSeconds
  return $ gameLoop (newWorld :: World) time frameTime

runEventPipe :: Producer -> Consumer -> IO ()
runEventPipe prod cons = do
  m <- newEmptyMVar
  let p = EventPipe m
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
  get "/" $ file "front/index.html"

wsapp :: WS.ServerApp
wsapp pending = do
  conn <- WS.acceptRequest pending
  putStrLn "New connection"
  WS.forkPingThread conn 30
  game <- newGame
  putStrLn "New game"
  runEventPipe (echoProd conn) (game conn)

echoProd :: WS.Connection -> EventPipe -> IO ()
echoProd conn (EventPipe m) = forever $ do
  msg <- WS.receiveData conn
  putMVar m (Message msg)

echoCons :: WS.Connection -> EventPipe -> IO ()
echoCons conn (EventPipe m) = loop
    where loop = do
            evt <- takeMVar m
            case evt of
              Message msg -> do
                       WS.sendTextData conn $ msg `T.append` " <-"
                       loop
              Stop s -> do
                       putMVar s ()
