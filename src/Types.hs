{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import GHC.Generics

import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen )

import qualified Data.ByteString as BS

type Coord = (Int, Int)
type Path = [Coord]

type GridDimensions = (Int, Int)

data GameParams = GameParams GridDimensions
                  deriving ( Show, Eq, Generic )

data World = World
  { grid :: Grid
  , player :: Object
  , enemy :: Object
  } deriving ( Show, Eq, Generic )

type Grid = [[Cell]]

data Cell = Empty
          | Wall
          deriving ( Show, Eq, Generic )

data Object = Object
  { pos :: Position
  , dst :: Position
  } deriving ( Show, Eq, Generic )

type Position = V2 Int

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
