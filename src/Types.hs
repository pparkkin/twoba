{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import GHC.Generics

import Linear.V2 ( V2(V2) )
import System.Random ( RandomGen )

import qualified Data.ByteString as BS
import qualified Data.Text as T

type Coord = (Int, Int)
type Path = [Coord]

type GridDimensions = (Int, Int)

data GameParams = GameParams GridDimensions
                  deriving ( Show, Eq, Generic )

data World = World
  { grid :: Grid
  , players :: [(PlayerName, ActiveObject)]
  } deriving ( Show, Eq, Generic )

type Grid = [[Cell]]

data Cell = Empty
          | Wall
          deriving ( Show, Eq, Generic )

type PlayerName = T.Text

data Object = Object
  { pos :: Position
  } deriving ( Show, Eq, Generic )
data ActiveObject = ActiveObject
  { pos :: Position
  , dst :: Position
  , speed :: Int
  , cooldown :: Int
  } deriving ( Show, Eq, Generic )

type Position = V2 Int

type Second = Double

class Game a where
  data Params a :: *
  input :: PlayerName -> BS.ByteString -> a -> a
  update :: Second -> a -> a
  newWorld :: RandomGen g => Params a -> g -> a

data Message = ClientHello ClientInfo
             | ServerHello GameParams
             | ServerState World
             | AddPlayer
             | PlayerMove Position
             deriving ( Show, Eq, Generic )

type ClientInfo = String
