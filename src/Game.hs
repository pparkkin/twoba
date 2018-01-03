{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Game where

import Debug.Trace ( trace )

import Data.List ( find )
import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Message
import PathFinding
import Serializable
import Types
import World

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

isEnemy  :: World -> PlayerName -> Position -> Bool
isEnemy w n p =
  case enemyObjectFor n w of
    Nothing -> False
    Just e -> p == pos (e :: Object)

isWall :: World -> Position -> Bool
isWall w p = not $ canMoveTo w (x, y)
  where
    (V2 x y) = p

canMove :: World -> (PlayerName, ActiveObject) -> Position -> Bool
canMove w (n, _) p = not (isEnemy w n p) && not (isWall w p)

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

movePlayers :: World -> World
movePlayers w@(World _ _ ps) = w { players = ps' }
  where
    ps' = map (movePlayer w) ps

attackPlayers :: World -> World
attackPlayers w@(World _ _ ps@(p1:p2:[])) = w { players = ps' }
  where
    ps' = if playerDistance p1 p2 <= 1
            then map (decrementPlayerHp 1) ps
            else ps
attackPlayers w = w

isDead :: Player -> Bool
isDead (Player _ o) = hp o <= 0

isGameOver :: World -> Bool
isGameOver (World _ _ ps) = or (map isDead ps)

updateWorld :: World -> World
updateWorld w =
  if isGameOver w
    then w
    else attackPlayers . movePlayers $ w

setObjectDest :: Position -> ActiveObject -> ActiveObject
setObjectDest p o = o { dst = p }

setPlayerDest :: PlayerName -> Position -> World -> World
setPlayerDest n p@(V2 x y) w@(World _ _ ps) = w { players = aux ps }
  where
    aux [] = []
    aux ((Player n' o):rs)
      | n' == n = (Player n' (setDest o)) : rs
      | otherwise = (Player n' o) : aux rs
    setDest o =
      if not (isWall w p) && pathLength w ep cur p <= speed o
        then setObjectDest p o
        else o
      where
        cur = pos (o :: ActiveObject)
        ep = moveBlockListCFor n w

addPlayer :: PlayerName -> World -> World
addPlayer n w@(World _ _ ps)
  | length ps == 0 = w { players = initPlayer FirstPlayer n : ps }
  | length ps == 1 = w { players = initPlayer SecondPlayer n : ps }
  | otherwise = w

handleInput :: PlayerName -> Message -> World -> World
handleInput n (PlayerMove p) w = setPlayerDest n p w
handleInput n AddPlayer w = addPlayer n w
handleInput _ _ w = w

playerLookup :: PlayerName -> [Player] -> Maybe Player
playerLookup _ [] = Nothing
playerLookup n (p:ps)
  | name p == n = Just p
  | otherwise = playerLookup n ps

playerObjectFor :: PlayerName -> World -> ActiveObject
playerObjectFor n w =
  case playerLookup n (players w) of
    Just a -> object a

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

projectWorld :: PlayerName -> World -> WorldProjection
projectWorld n w = WorldProjection player enemy
  where
    player = playerObjectFor n w
    enemy = enemyObjectFor n w

worldInit :: World -> WorldInit
worldInit (World params grid _) = WorldInit params grid

instance Game World where
  data Params World = Params GameParams
  input n bs world =
    case deserialize bs of
      Just m -> handleInput n m world
      Nothing -> world
  output n = serialize . ServerState . projectWorld n
  update _ = updateWorld
  newWorld (Params p@(GameParams (x, y))) seed = World.newWorld p seed x y
