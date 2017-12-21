{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Game where

import Debug.Trace ( trace )

import GHC.Generics

import Data.List ( find )
import Data.List.Split ( chunksOf )
import Data.Tuple ( swap )
import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen
                     , randomRs
                     )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as S

import Message
import Serializable
import Types

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
    then Wall
    else (os !! y) !! x

neighbors :: World -> (Int, Int) -> [Cell]
neighbors w = map (cellAt w) . neighborCoords

updateWorld :: World -> World
updateWorld = id

moveObject :: Position -> Object -> Object
moveObject p o = o { pos = p }

movePlayer :: World -> Position -> Object -> Object
movePlayer w p@(V2 x y) o =
  case cellAt w ((round x), (round y)) of
    Empty -> moveObject p o
    Wall -> o

handleInput :: Message -> World -> World
handleInput (PlayerMove p) w = w { player = movePlayer w p (player w) }
handleInput _ w = w

fillGrid :: Cell -> Int -> Int -> Grid
fillGrid c x y = chunksOf x . take (x * y) . repeat $ c

inGrid :: Int -> Int -> (Int, Int) -> Bool
inGrid x y (x', y') = x' >= 0 && x' < x && y' >= 0 && y' < y

diagonalPath :: Int -> Int -> [(Int, Int)]
diagonalPath 0 0 = []
diagonalPath x y = (x-1, y-2) : (x-1, y-1) : (x-1, y) : diagonalPath (x-1) (y-1)

generateGrid :: RandomGen g => g -> Int -> Int -> Grid
generateGrid seed x y =
  chunksOf x $ pickCells es cs
  where
    cs = [ (c, r) | c <- [0..(y-1)], r <- [0..(x-1)]]
    p = S.fromList $ diagonalPath x y
    es = S.filter (inGrid x y) p
    pickCells _ [] = []
    pickCells s (c:cs)
      | S.null s = Wall : pickCells s cs
      | otherwise =
        if S.member (swap c) s
          then Empty : pickCells (S.delete (swap c) s) cs
          else Wall : pickCells s cs

initPlayer :: Object
initPlayer = Object (V2 0.0 0.0) (V2 0.0 0.0)

instance Game World where
  data Params World = Params GameParams
  input bs world =
    case deserialize bs of
      Just m -> handleInput m world
      Nothing -> world
  update _ world = updateWorld world
  newWorld (Params (GameParams (x, y))) seed = World (generateGrid seed x y) initPlayer
