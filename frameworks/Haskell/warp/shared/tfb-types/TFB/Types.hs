{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}

module TFB.Types
  ( unsafeJsonString,
    parseCount,
    getCount,
    Count,
    World (..),
    Fortune (..),
    QId,
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as Parsec
import Data.BufferBuilder.Json ((.=))
import qualified Data.BufferBuilder.Json as Json
import qualified Data.BufferBuilder.Utf8 as Utf8
import Data.ByteString (ByteString)
import qualified Data.Either as Either
import Data.Text (Text)

-------------------------------------------------------------------------------
-- * Inputs

newtype Count = Count Int

parseCount :: ByteString -> Maybe Count
parseCount = fmap Count . Either.either (const Nothing) pure . Parsec.parseOnly Parsec.decimal

getCount :: Maybe Count -> Int
getCount Nothing = 1
getCount (Just (Count c)) = max 1 (min c 500)

type QId = Int

-------------------------------------------------------------------------------
-- * Outputs

data World = World {wId :: QId, wRandomNumber :: QId}
  deriving (Show)

instance Json.ToJson World where
  toJson w =
    Json.toJson $
      "id" .= wId w
        <> "randomNumber" .= wRandomNumber w

data Fortune = Fortune {fId :: QId, fMessage :: Text}
  deriving (Show)

unsafeJsonString :: ByteString -> Json.Value
unsafeJsonString = Json.unsafeValueUtf8Builder . Utf8.appendBS7 . quote
  where
    quote x = "\"" <> x <> "\""
