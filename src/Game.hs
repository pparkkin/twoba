{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Game where

import Debug.Trace ( trace )

import GHC.Generics

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

updatePlayer :: World -> ActiveObject -> ActiveObject
updatePlayer w p@(ActiveObject l d _ c)
  | l == d = decrementCooldown p
  | c > 0 = decrementCooldown p
  | otherwise = p { pos = d, cooldown = 12 }

updateEnemy :: World -> Object -> Object
updateEnemy _ e = e

updateWorld :: World -> World
updateWorld w@(World _ p e) =
  w { player = updatePlayer w p
    , enemy = updateEnemy w e
    }

moveObject :: World -> Position -> ActiveObject -> ActiveObject
moveObject w p@(V2 x' y') o =
  if length path > speed o
    then o
    else o { dst = p }
  where
    path = findPath w (x, y) (x', y')
    (V2 x y) = pos (o :: ActiveObject)

movePlayer :: World -> Position -> ActiveObject -> ActiveObject
movePlayer w p@(V2 x y) o =
  case cellAt w (x, y) of
    Empty -> moveObject w p o
    Wall -> o

handleInput :: Message -> World -> World
handleInput (PlayerMove p) w = w { player = movePlayer w p (player w) }
handleInput _ w = w

instance Game World where
  data Params World = Params GameParams
  input bs world =
    case deserialize bs of
      Just m -> handleInput m world
      Nothing -> world
  update _ world = updateWorld world
  newWorld (Params (GameParams (x, y))) seed = World.newWorld seed x y
