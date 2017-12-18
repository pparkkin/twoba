{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Game where

import GHC.Generics

import Data.Aeson ( ToJSON
                  , toJSON
                  , object
                  , (.=)
                  , encode
                  )
import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

import Serializable

data World = World {
      objects :: [ Object ]
} deriving ( Show, Generic )

data Object = Object {
      pos :: Position,
      vel :: Velocity
} deriving ( Show, Generic )

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

instance Game World where
  update world dt = world { objects = os' }
    where
      os' = map updateObject os
      os  = objects world
      updateObject o = o { pos = (pos o) + (vel o) }
  newWorld seed = World [ Object pos vel ]
    where pos = V2 0.0 0.0
          vel = V2 0.0 0.0
