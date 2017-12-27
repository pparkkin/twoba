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

updatePlayer :: World -> ActiveObject -> ActiveObject
updatePlayer w p@(ActiveObject l d c) =
  if l == d
    then p
    else p { pos = V2 x'' y'' }
      where
        (V2 x y) = l
        (V2 x' y') = d
        (x'', y'') = head $ findPath w (x, y) (x', y')

updateEnemy :: World -> Object -> Object
updateEnemy _ e = e

updateWorld :: World -> World
updateWorld w@(World _ p e) =
  w { player = updatePlayer w p
    , enemy = updateEnemy w e
    }

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
