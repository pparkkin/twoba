module Game where

import Linear.V2 ( V2(V2) )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Serializable

data World = World {
      objects :: [ Object ]
}

data Object = Object {
      pos :: Position,
      vel :: Velocity
} deriving (Show)

type Position = V2 Double
type Velocity = V2 Double

instance Serializable World where
  serialize w = BS.concat $ map (C8.pack . show) (objects w)

type Second = Double

class Game a where
  update :: a -> Second -> a
  newWorld :: a

instance Game World where
  update world dt = world { objects = os' }
    where
      os' = map updateObject os
      os  = objects world
      updateObject o = o { pos = (pos o) + (vel o) }
  newWorld = World [ Object pos vel ]
    where pos = V2 0.0 0.0
          vel = V2 1.0 0.0
