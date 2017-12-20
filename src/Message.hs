{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Message where

import GHC.Generics

import Data.Aeson ( ToJSON
                  , encode
                  )
import qualified Data.ByteString.Lazy as BL

import Client
import Game
import Serializable

data Message = ClientHello ClientInfo
             | ServerHello GameParams
             | ServerState World
             deriving ( Show, Eq, Generic )

instance ToJSON Message
-- instance FromJSON Message

instance Serializable Message where
  serialize = BL.toStrict . encode
