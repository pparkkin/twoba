{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson ( ToJSON, FromJSON
                  , toJSON, parseJSON
                  , encode, decode
                  , object, withObject
                  , (.=), (.:)
                  )
import Linear.V2 ( V2(V2) )

import qualified Data.ByteString.Lazy as BL

import Serializable
import Types

instance ToJSON GameParams
instance FromJSON GameParams

instance ToJSON a => ToJSON (V2 a) where
  toJSON (V2 x y) =
    object ["x" .= x, "y" .= y]
instance FromJSON a => FromJSON (V2 a) where
  parseJSON = withObject "V2" $ \v -> V2
    <$> v .: "x"
    <*> v .: "y"

instance ToJSON Cell
instance FromJSON Cell
instance ToJSON Object
instance FromJSON Object
instance ToJSON ActiveObject
instance FromJSON ActiveObject
instance ToJSON World
instance FromJSON World

instance ToJSON Message
instance FromJSON Message

instance Serializable Message where
  serialize = BL.toStrict . encode
  deserialize = decode . BL.fromStrict
