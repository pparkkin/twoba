module Serializable where

import Data.ByteString ( ByteString )

class Serializable a where
  serialize :: a -> ByteString
