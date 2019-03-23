{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Lib.Types (
    unsafeJsonString
  , parseCount
  , getCount
  , Count
  , World(..)
  , Fortune(..)
  , FortunesHtml
  , QId
) where

import qualified Data.Either as Either
import qualified Data.Char as Char
import           Data.Word (Word16)

import           Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString.Char8 as Parsec
import qualified Data.BufferBuilder.Utf8 as Utf8
import qualified Data.BufferBuilder.Json as Json
import           Data.BufferBuilder.Json ((.=))
import qualified Html
import           Html (type (#), type (>))
import           Data.Text (Text)

-------------------------------------------------------------------------------
-- * Inputs

newtype Count = Count Int

parseCount :: ByteString -> Maybe Count
parseCount = fmap Count . Either.either (const Nothing) pure . Parsec.parseOnly parseInt

getCount :: Maybe Count -> Int
getCount Nothing = 1
getCount (Just (Count c)) = max 1 (min c 500)

-- https://stackoverflow.com/a/24171263
parseInt :: Parsec.Parser Int
parseInt = do
  digits <- Parsec.many1 parseIntDigit
  let n = foldl (\x d -> 10*x + (Char.digitToInt d)) 0 digits
  seq n (return n)

parseIntDigit :: Parsec.Parser Char
parseIntDigit = digit
  where
    digit = Parsec.satisfy isDigit
    isDigit c = c >= '0' && c <= '9'

type QId = Word16

-------------------------------------------------------------------------------
-- * Outputs

data World = World { wId :: Int , wRandomNumber :: Int }
  deriving Show

instance Json.ToJson World where
  toJson w
    = Json.toJson
    $ "id"           .= wId w
   <> "randomNumber" .= wRandomNumber w

data Fortune = Fortune { fId :: Int , fMessage :: Text }
  deriving Show

instance Json.ToJson Fortune where
  toJson f
    = Json.toJson
    $ "id"      .= fId f
   <> "message" .= fMessage f

type FortunesHtml
  = (('Html.DOCTYPE Html.> ())
  # ('Html.Html
    > (('Html.Head > ('Html.Title > Html.Raw Text))
      # ('Html.Body
        > ('Html.Table
          > (
              ('Html.Tr
              > ( ('Html.Th > Html.Raw Text)
                # ('Html.Th > Html.Raw Text)
                )
              )
            # ['Html.Tr
              > ( ('Html.Td > Int)
                # ('Html.Td > Text)
                )
              ]
            )
          )
        )
      )
    )
  )

unsafeJsonString :: ByteString -> Json.Value
unsafeJsonString = Json.unsafeValueUtf8Builder . Utf8.appendBS7 . quote
  where
    quote x = "\"" <> x <> "\""
