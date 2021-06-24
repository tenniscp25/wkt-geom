module Data.Hex
  ( safeConvert
  , Hex(..)
  ) where

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base16 as ByteStringBase16
import qualified Data.ByteString.Char8  as ByteStringChar8
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.Geospatial        as Geospatial
import           Data.Monoid            ((<>))

newtype Hex = Hex ByteString.ByteString

safeConvert :: (LazyByteString.ByteString -> Either String Geospatial.GeospatialGeometry) -> Hex -> Either String Geospatial.GeospatialGeometry
safeConvert f (Hex byteString) =
  case ByteStringBase16.decode byteString of
    Left e -> Left $ "Invalid hex representation: " <> e
    Right a -> f $ LazyByteString.fromStrict a
