{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Game where

import Debug.Trace ( trace )

import Control.Monad ( when
                     , unless
                     )
import Control.Monad.Trans.State ( StateT )
import Data.List ( find )
import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen)

import qualified Control.Monad.Trans.State as ST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Message
import PathFinding
import Serializable
import Types
import World

type Game a = StateT World IO a

decrementCooldown :: ActiveObject -> ActiveObject
decrementCooldown o@(ActiveObject _ _ _ c _)
  | c <= 0 = o { cooldown = 0 }
  | otherwise = o { cooldown = c - 1 }

decrementHp :: Int -> ActiveObject -> ActiveObject
decrementHp n o@(ActiveObject _ _ _ _ h)
  | h <= 0 = o { hp = 0 }
  | otherwise = o { hp = h - n }

decrementPlayerHp :: Int -> Player -> Player
decrementPlayerHp m (Player n o) = (Player n (decrementHp m o))

resetCooldown :: ActiveObject -> ActiveObject
resetCooldown o = o { cooldown = 12 }

moveObject :: Position -> ActiveObject -> ActiveObject
moveObject d o = o { pos = d }

resetDestination :: ActiveObject -> ActiveObject
resetDestination o = o { dst = pos (o :: ActiveObject) }

isEnemy  :: World -> Position -> PlayerName -> Bool
isEnemy w p n =
  case enemyObjectFor n w of
    Nothing -> False
    Just e -> p == pos (e :: Object)

isWall :: World -> Position -> Bool
isWall w p = not $ canMoveTo w (x, y)
  where
    (V2 x y) = p

canMove :: World -> (PlayerName, ActiveObject) -> Position -> Bool
canMove w (n, _) p = not (isEnemy w p n) && not (isWall w p)

canSetDest :: World -> Position -> Bool
canSetDest w p = not (isWall w p)

objectDistance :: ActiveObject -> ActiveObject -> Int
objectDistance o1 o2 = distance (x1, y1) (x2, y2)
  where
    (V2 x1 y1) = pos (o1 :: ActiveObject)
    (V2 x2 y2) = pos (o2 :: ActiveObject)

playerDistance :: Player -> Player -> Int
playerDistance (Player _ o1) (Player _ o2) = objectDistance o1 o2

movePlayer :: World -> Player -> Player
movePlayer w p@(Player n o@(ActiveObject l d _ c _))
  | l == d = p { object = decrementCooldown o }
  | c > 0 = p { object = decrementCooldown o }
  | otherwise = p { object = snd $ foldl stepObject (False, o) path }
    where
      path = findPath w ep (x, y) (x', y')
      (V2 x y) = l
      (V2 x' y') = d
      ep = moveBlockListCFor n w
      stepObject :: (Bool, ActiveObject) -> Coord -> (Bool, ActiveObject)
      stepObject a@(True, _) _ = a -- Skip the rest
      stepObject (_, o) (x, y)
        | canMove w (n, o) (V2 x y) = (False, resetCooldown . moveObject (V2 x y) $ o)
        | otherwise = (True, decrementCooldown . resetDestination $ o)

movePlayers :: Game ()
movePlayers = do
  w <- ST.get
  let
    ps = players w
    ps' = map (movePlayer w) ps
  ST.put $ w { players = ps' }

attackPlayers :: Game ()
attackPlayers = do
  w <- ST.get
  when (length (players w) == 2) $ do
    let
      ps@(p1:p2:[]) = players w
      ps' = if playerDistance p1 p2 <= 1
              then map (decrementPlayerHp 1) ps
              else ps
    ST.put $ w { players = ps' }

isDead :: Player -> Bool
isDead (Player _ o) = hp o <= 0

isGameOver :: World -> Bool
isGameOver (World _ _ ps) = or (map isDead ps)

updateWorld :: Game ()
updateWorld = do
  w <- ST.get
  unless (isGameOver w) $ do
    movePlayers
    attackPlayers

setObjectDest :: Position -> ActiveObject -> ActiveObject
setObjectDest p o = o { dst = p }

setPlayerDest :: PlayerName -> Position -> Game ()
setPlayerDest n p@(V2 x y) = do
  w <- ST.get
  ST.put $ w { players = aux w (players w) }
  where
    aux _ [] = []
    aux w ((Player n' o):rs)
      | n' == n = (Player n' (setDest w o)) : rs
      | otherwise = (Player n' o) : aux w rs
    setDest w o =
      if canSetDest w p && pathLength w ep cur p <= speed o
        then setObjectDest p o
        else o
      where
        cur = pos (o :: ActiveObject)
        ep = moveBlockListCFor n w

addPlayer :: PlayerName -> Game ()
addPlayer n = do
  w <- ST.get
  case length (players w) of
    0 -> ST.put $ w { players = initPlayer FirstPlayer n : (players w) }
    1 -> ST.put $ w { players = initPlayer SecondPlayer n : (players w) }
    _ -> return ()

handleInput :: PlayerName -> Message -> Game ()
handleInput n AddPlayer = do
  w <- ST.get
  unless (isGameOver w) (addPlayer n)
handleInput n (PlayerMove p) = do
  w <- ST.get
  unless (isGameOver w) (setPlayerDest n p)
handleInput _ _ = return ()

playerLookup :: PlayerName -> [Player] -> Maybe Player
playerLookup _ [] = Nothing
playerLookup n (p:ps)
  | name p == n = Just p
  | otherwise = playerLookup n ps

playerObjectFor :: PlayerName -> Game ActiveObject
playerObjectFor n = do
  w <- ST.get
  case playerLookup n (players w) of
    Just a -> return $ object a

moveBlockListFor :: PlayerName -> World -> [Position]
moveBlockListFor n w =
  case enemyObjectFor n w of
    Nothing -> []
    Just e -> [pos (e :: Object)]

moveBlockListCFor :: PlayerName -> World -> [Coord]
moveBlockListCFor n w = map (\(V2 x y) -> (x, y)) (moveBlockListFor n w)

-- NOTE: Following assumes two players only
enemyObjectFor :: PlayerName -> World -> Maybe Object
enemyObjectFor n w = enemyObject (players w)
  where
    enemyObject [] = Nothing
    enemyObject (p:ps)
      | name p /= n = Just $ Object (pos (object p :: ActiveObject)) (hp (object p) <= 0)
      | otherwise = enemyObject ps

projectWorld :: PlayerName -> Game WorldProjection
projectWorld n = do
  player <- playerObjectFor n
  w <- ST.get
  let
    enemy = enemyObjectFor n w
  return $ WorldProjection player enemy

worldInit :: World -> WorldInit
worldInit (World params grid _) = WorldInit params grid

input :: PlayerName -> BS.ByteString -> Game ()
input n bs =
  case deserialize bs of
    Just m -> handleInput n m
    Nothing -> return ()

output :: PlayerName -> Game BS.ByteString
output n = do
  p <- projectWorld n
  return $ serialize (ServerState p)

update :: Second -> Game ()
update _ = updateWorld

newWorld :: RandomGen g => GameParams -> g -> World
newWorld p@(GameParams (x, y)) seed = World.newWorld p seed x y
