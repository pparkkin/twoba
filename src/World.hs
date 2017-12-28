{-# LANGUAGE OverloadedStrings #-}

module World where

import Data.Bits ( xor )
import Data.List.Split ( chunksOf )
import Data.Tuple ( swap )
import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen
                     , randomRs
                     )

import qualified Data.Set as S
import qualified Data.Text as T

import Types

newWorld :: RandomGen g => g -> Int -> Int -> World
newWorld seed x y =
  World (generateGrid seed x y) []

fillGrid :: Cell -> Int -> Int -> Grid
fillGrid c x y = chunksOf x . replicate (x * y) $ c

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

data PlayerSlot = FirstPlayer | SecondPlayer
                deriving ( Eq, Ord, Show )

initPlayer :: PlayerSlot -> PlayerName -> (PlayerName, ActiveObject)
initPlayer FirstPlayer n = (n, ActiveObject topLeft topLeft 5 0)
  where topLeft = V2 0 0
initPlayer SecondPlayer n = (n, ActiveObject bottomRight bottomRight 5 0)
  -- FIXME: Don't use hard coded grid size
  where bottomRight = V2 19 19

cellAt :: World -> Coord -> Cell
cellAt (World os _) (x, y) =
  if (x < 0 || y < 0) || (x > 19 || y > 19)
    then Wall
    else (os !! y) !! x

cellsAt :: World -> [(Int, Int)] -> [Cell]
cellsAt _ [] = []
cellsAt w (c:cs) = cellAt w c : cellsAt w cs

canMoveToCell :: Cell -> Bool
canMoveToCell Wall = False
canMoveToCell Empty = True

canMoveTo :: World -> Coord -> Bool
canMoveTo w p = canMoveToCell $ cellAt w p

openNeighbors :: World -> Coord -> [Coord]
openNeighbors w = filter (canMoveTo w) . neighbors fourDirs

neighborCells :: World -> (Int, Int) -> [Cell]
neighborCells w = map (cellAt w) . allNeighbors

eightDirs :: [(Int, Int)]
eightDirs =
  filter (\(x, y) -> x /= 0 || y /= 0) [
    (x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]
  ]

fourDirs :: [(Int, Int)]
fourDirs =
  filter (\(x, y) -> (x == 0) `xor` (y == 0)) [
    (x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]
  ]

neighbors :: [(Int, Int)] -> Coord -> [Coord]
neighbors ns (x, y) =
  map (\(x', y') -> (x + x', y + y')) ns

allNeighbors :: Coord -> [Coord]
allNeighbors = neighbors eightDirs
