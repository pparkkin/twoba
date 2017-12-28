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

updatePlayer :: World -> (PlayerName, ActiveObject) -> (PlayerName, ActiveObject)
updatePlayer w (n, o@(ActiveObject l d _ c))
  | l == d = (n, decrementCooldown o)
  | c > 0 = (n, decrementCooldown o)
  | otherwise = (n, resetCooldown . moveObject d $ o)

updateEnemy :: World -> Object -> Object
updateEnemy _ e = e

updateWorld :: World -> World
updateWorld w@(World _ p e) =
  w { player = updatePlayer w p
    , enemy = updateEnemy w e
    }

setObjectDest :: Position -> ActiveObject -> ActiveObject
setObjectDest p o = o { dst = p }

setPlayerDest :: World -> Position -> (PlayerName, ActiveObject) -> (PlayerName, ActiveObject)
setPlayerDest w p@(V2 x y) (n, o) =
  if canMoveTo w (x, y) && pathLength w cur p <= speed o
    then (n, setObjectDest p o)
    else (n, o)
  where
    cur = pos (o :: ActiveObject)

handleInput :: Message -> World -> World
handleInput (PlayerMove p) w = w { player = setPlayerDest w p (player w) }
handleInput _ w = w

instance Game World where
  data Params World = Params GameParams
  input bs world =
    case deserialize bs of
      Just m -> handleInput m world
      Nothing -> world
  update _ = updateWorld
  newWorld (Params (GameParams (x, y))) seed = World.newWorld seed x y
