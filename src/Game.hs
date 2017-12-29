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
decrementCooldown o@(ActiveObject _ _ _ c)
  | c <= 0 = o { cooldown = 0 }
  | otherwise = o { cooldown = c - 1 }

resetCooldown :: ActiveObject -> ActiveObject
resetCooldown o = o { cooldown = 12 }

moveObject :: Position -> ActiveObject -> ActiveObject
moveObject d o = o { pos = d }

isDestinationEnemy  :: World -> (PlayerName, ActiveObject) -> Bool
isDestinationEnemy w (n, o) =
  case enemyObjectFor n w of
    Nothing -> False
    Just e -> enemyPos == ownDst
      where
        enemyPos = pos (e :: Object)
        ownDst = dst (o :: ActiveObject)

isDestinationWall :: World -> (PlayerName, ActiveObject) -> Bool
isDestinationWall w (n, o) = not $ canMoveTo w (x, y)
  where
    (V2 x y) = dst (o :: ActiveObject)

canMove :: World -> (PlayerName, ActiveObject) -> Bool
canMove w p = not (isDestinationEnemy w p) && not (isDestinationWall w p)

updatePlayer :: World -> (PlayerName, ActiveObject) -> (PlayerName, ActiveObject)
updatePlayer w p@(n, o@(ActiveObject l d _ c))
  | l == d = (n, decrementCooldown o)
  | c > 0 = (n, decrementCooldown o)
  | canMove w p = (n, resetCooldown . moveObject d $ o)
  | otherwise = (n, decrementCooldown o)

updatePlayers :: World -> [(PlayerName, ActiveObject)] -> [(PlayerName, ActiveObject)]
updatePlayers w = map (updatePlayer w)

updateWorld :: World -> World
updateWorld w@(World _ _ p) =
  w { players = updatePlayers w p
    }

setObjectDest :: Position -> ActiveObject -> ActiveObject
setObjectDest p o = o { dst = p }

setPlayerDest :: PlayerName -> Position -> World -> World
setPlayerDest n p@(V2 x y) w@(World _ _ ps) = w { players = aux ps }
  where
    aux [] = []
    aux ((n',o):rs)
      | n' == n = (n', setDest o) : rs
      | otherwise = (n', o) : aux rs
    setDest o =
      if canMoveTo w (x, y) && pathLength w cur p <= speed o
        then setObjectDest p o
        else o
      where
        cur = pos (o :: ActiveObject)

addPlayer :: PlayerName -> World -> World
addPlayer n w@(World _ _ ps)
  | length ps == 0 = w { players = initPlayer FirstPlayer n : ps }
  | length ps == 1 = w { players = initPlayer SecondPlayer n : ps }
  | otherwise = w

handleInput :: PlayerName -> Message -> World -> World
handleInput n (PlayerMove p) w = setPlayerDest n p w
handleInput n AddPlayer w = addPlayer n w
handleInput _ _ w = w

playerObjectFor :: PlayerName -> World -> ActiveObject
playerObjectFor n w =
  case lookup n (players w) of
    Just a -> a

enemyObjectFor :: PlayerName -> World -> Maybe Object
enemyObjectFor n w = enemyObject (players w)
  where
    enemyObject [] = Nothing
    enemyObject ((n',o):ps)
      | n' /= n = Just $ Object (pos (o :: ActiveObject))
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
