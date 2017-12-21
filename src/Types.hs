{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import GHC.Generics

import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen )

import qualified Data.ByteString as BS

type GridDimensions = (Int, Int)

data GameParams = GameParams GridDimensions
                  deriving ( Show, Eq, Generic )

data World = World
  { grid :: [[ Cell ]]
  , player :: Object
  } deriving ( Show, Eq, Generic )

data Cell = Live
          | Dead
          deriving ( Show, Eq, Generic )

data Object = Object
  { pos :: Position
  , vel :: Velocity
  } deriving ( Show, Eq, Generic )

type Position = V2 Double
type Velocity = V2 Double

type Second = Double

class Game a where
  data Params a :: *
  input :: BS.ByteString -> a -> a
  update :: Second -> a -> a
  newWorld :: RandomGen g => Params a -> g -> a

data Message = ClientHello ClientInfo
             | ServerHello GameParams
             | ServerState World
             | PlayerMove Position
             deriving ( Show, Eq, Generic )

type ClientInfo = String
