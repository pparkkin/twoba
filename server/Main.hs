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

-- http://chimera.labs.oreilly.com/books/1230000000929/ch07.html
data EventPipe = EventPipe (MVar Event)
data Event = Message T.Text | Stop (MVar ())

type Consumer = (EventPipe -> IO ())
type Producer = (EventPipe -> IO ())

-- http://www.thesoftwaredevelopmentlifecycle.com/writing-a-game-loop-in-haskell/
data World = World {
      objects :: [ Object ]
}
data Object = Object {
      pos :: Position,
      vel :: Velocity
} deriving (Show)
type Position = Vector2D
type Velocity = Vector2D
data Vector2D = Vector2D { x :: Double, y :: Double } deriving (Show)

addVec2D :: Vector2D -> Vector2D -> Vector2D
addVec2D v1 v2 = Vector2D ((x v1) + (x v2)) ((y v1) + (y v2))

update :: World -> Second -> Second -> World
update world timeSinceStart delta = world { objects = os' }
    where os' = map updateObject os
          os  = objects world
          updateObject o = o { pos = addVec2D (pos o) (vel o) }

type Second = Double

updateLoop :: World -> Second -> Second -> Second -> (World, World, Second, Second)
updateLoop world timeSinceStart delta timeRemaining =
    let world' = update world timeSinceStart delta
    in if timeRemaining >= delta
       then updateLoop world' (timeSinceStart + delta) delta (timeRemaining - delta)
       else (world', world, timeSinceStart, timeRemaining)

serialize :: World -> T.Text
serialize w = T.concat $ map (T.pack . show) (objects w)

display :: WS.Connection -> World -> IO ()
display conn world = WS.sendTextData conn (serialize world)

gameLoop :: World -> Second -> Second -> Second -> Second -> WS.Connection -> EventPipe -> IO ()
gameLoop world elapsedTime delta currentTime acc conn input = do
  newTime <- getTimeInSeconds
  putStrLn $ printf "Current loop time stamp %.4f" newTime
  let frameTime = max 0.25 $ newTime - currentTime
      (previousWorld, currentWorld, elapsedTime', acc') =
        updateLoop world elapsedTime delta (acc + frameTime)
  display conn currentWorld
  gameLoop currentWorld elapsedTime' delta newTime acc' conn input

getTimeInSeconds :: IO Double
getTimeInSeconds = do
  t <- getCPUTime
  return ((fromIntegral t) / 10^12)

newWorld :: World
newWorld = World [ Object pos vel ]
    where pos = Vector2D 0.0 0.0
          vel = Vector2D 1.0 0.0

newGame :: IO (WS.Connection -> EventPipe -> IO ())
newGame = do
  time <- getTimeInSeconds
  return $ gameLoop newWorld 0.0 0.1 time 0.0

runEventPipe :: Producer -> Consumer -> IO ()
runEventPipe prod cons = do
  m <- newEmptyMVar
  let p = EventPipe m
  forkIO (cons p)
  prod p

main :: IO ()
main = do
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
