{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Game where

import Debug.Trace ( trace )

import GHC.Generics

import Data.List ( find )
import Data.List.Split ( chunksOf )
import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen
                     , randomRs
                     )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Message
import Serializable
import Types

isLive :: Cell -> Bool
isLive Live = True
isLive Dead = False

eightDirs :: [(Int, Int)]
eightDirs =
  filter (\(x, y) -> x /= 0 || y /= 0) [
    (x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]
  ]

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x, y) =
  map (\(x', y') -> (x + x', y + y')) eightDirs

cellAt :: World -> (Int, Int) -> Cell
cellAt (World os _) (x, y) =
  if (x < 0 || y < 0) || (x > 19 || y > 19)
    then Dead
    else (os !! y) !! x

neighbors :: World -> (Int, Int) -> [Cell]
neighbors w = map (cellAt w) . neighborCoords

updateWorld :: World -> World
updateWorld = id

moveObject :: Position -> Object -> Object
moveObject p o = o { pos = p }

handleInput :: Message -> World -> World
handleInput (PlayerMove p) w = w { player = moveObject p (player w) }
handleInput _ w = w

instance Game World where
  data Params World = Params GameParams
  input bs world =
    case deserialize bs of
      Just m -> handleInput m world
      Nothing -> world
  update _ world = updateWorld world
  newWorld (Params (GameParams (x, y))) seed = World (chunksOf x cells) p
    where
      cells = map (kill 0.7) (take (x * y) rs)
      rs = randomRs (0,1) seed :: [Float]
      kill p r = if r > p then Live else Dead
      p = Object (V2 0.0 0.0) (V2 0.0 0.0)
