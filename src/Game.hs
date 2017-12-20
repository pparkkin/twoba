{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Game where

import Debug.Trace ( trace )

import GHC.Generics

import Data.Aeson ( ToJSON
                  , toJSON
                  , object
                  , (.=)
                  , encode
                  )
import Data.List ( find )
import Data.List.Split ( chunksOf )
import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen
                     , randomRs
                     )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Serializable

type GridDimensions = (Int, Int)

data GameParams = GameParams GridDimensions
                  deriving ( Show, Eq, Generic )

instance ToJSON GameParams

data World = World
  { grid :: [[ Object ]]
  } deriving ( Show, Eq, Generic )

data Object = Live
            | Dead
            deriving ( Show, Eq, Generic )

type Position = V2 Double
type Velocity = V2 Double

instance ToJSON a => ToJSON (V2 a) where
  toJSON (V2 x y) =
    object ["x" .= x, "y" .= y]
instance ToJSON Object
instance ToJSON World

instance Serializable World where
  serialize = BL.toStrict . encode

type Second = Double

class Game a where
  update :: a -> Second -> a
  newWorld :: RandomGen g => g -> a

isLive :: Object -> Bool
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

cellAt :: World -> (Int, Int) -> Object
cellAt (World os) (x, y) =
  if (x < 0 || y < 0) || (x > 19 || y > 19)
    then Dead
    else (os !! y) !! x

neighbors :: World -> (Int, Int) -> [Object]
neighbors w = map (cellAt w) . neighborCoords

updateCell :: World -> Int -> Int -> Object
updateCell w x y =
  let
    c = cellAt w (x, y)
    ns = neighbors w (x, y)
    ls = filter isLive ns
  in
    case c of
      Live | length ls < 2 -> Dead
           | length ls > 3 -> Dead
           | otherwise -> Live
      Dead | length ls == 3 -> Live
           | otherwise -> Dead

updateRow :: World -> Int -> [Object] -> [Object]
updateRow w y os =
  map (\(x, c) -> updateCell w x y) (zip [0..] os)

updateGrid :: World -> World
updateGrid w@(World rs) =
  World $ map (\(y, r) -> updateRow w y r) (zip [0..] rs)

instance Game World where
  update world _ = updateGrid world
  newWorld seed = World (chunksOf 20 cells)
    where
      cells = map (kill 0.7) (take (20 * 20) rs)
      rs = randomRs (0,1) seed :: [Float]
      kill p r = if r > p then Live else Dead
