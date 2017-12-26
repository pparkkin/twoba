{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

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

updatePlayer :: World -> Object -> Object
updatePlayer w p@(Object c d) =
  if c == d
    then p
    else p { pos = V2 x'' y'' }
      where
        (V2 x y) = c
        (V2 x' y') = d
        (x'', y'') = head $ findPath w (x, y) (x', y')

updateEnemy :: World -> Object -> Object
updateEnemy _ e = e

updateWorld :: World -> World
updateWorld w@(World _ p e) =
  w { player = updatePlayer w p
    , enemy = updateEnemy w e
    }

moveObject :: Position -> Object -> Object
moveObject p o = o { dst = p }

movePlayer :: World -> Position -> Object -> Object
movePlayer w p@(V2 x y) o =
  case cellAt w (x, y) of
    Empty -> moveObject p o
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
