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
  { params :: GameParams
  , grid :: Grid
  , players :: [Player]
  } deriving ( Show, Eq, Generic )

data WorldInit = WorldInit
  { params :: GameParams
  , grid :: Grid
  } deriving ( Show, Eq, Generic )

data WorldProjection = WorldProjection
  { player :: ActiveObject
  , enemy :: Maybe Object
  } deriving ( Show, Eq, Generic )

type Grid = [[Cell]]

data Cell = Empty
          | Wall
          deriving ( Show, Eq, Generic )

data Player = Player
  { name :: PlayerName
  , object :: ActiveObject
  , hp :: Int
  } deriving ( Show, Eq, Generic )

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
  output :: PlayerName -> a -> BS.ByteString
  update :: Second -> a -> a
  newWorld :: RandomGen g => Params a -> g -> a

data Message = ClientHello ClientInfo
             | ServerHello WorldInit
             | ServerState WorldProjection
             | AddPlayer
             | PlayerMove Position
             deriving ( Show, Eq, Generic )

type ClientInfo = String
