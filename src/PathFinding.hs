module PathFinding where

import Debug.Trace ( trace )

import Data.Function ( on )
import Data.List ( sortBy )
import Linear.V2 ( V2(V2) )

import Types
import World

findPath :: World -> [Coord] -> Coord -> Coord -> Path
findPath w blocked start end
  | start == end = [start]
  | otherwise = reverse $ findPathStep w blocked (sortPaths end frontier) end
      where
        frontier = map (:[]) (openNeighbors w start)

pathLength :: World -> [Coord] -> Position -> Position -> Int
pathLength w blocked (V2 x y) (V2 x' y') = length $ findPath w blocked (x, y) (x', y')

findPathStep :: World -> [Coord] -> [Path] -> Coord -> Path
findPathStep w closed (p@(h:_):ps) end
  | h == end = p
  | h `elem` closed = findPathStep w closed ps end
  | otherwise = findPathStep w (h:closed) (sortPaths end frontier) end
      where
        newPaths = map (:p) (openNeighbors w h)
        frontier = newPaths ++ ps

sortPaths :: Coord -> [Path] -> [Path]
sortPaths e = sortBy (compare `on` scorePath e)

scorePath :: Coord -> Path -> Int
scorePath _ [] = maxBound
scorePath (x,y) p@((x',y'):_) = left + right
  where
    left = length p -- path length so far
    right = abs (x' - x) + abs (y' - y) -- estimated path remaining
