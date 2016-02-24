{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson     hiding (json)
import qualified Data.Text      as T
import           GHC.Exts
import           Web.Spock.Safe


main :: IO ()
main = runSpock 3000 $ spockT id $
        -- | Test 1: JSON serialization
    do  get "json" $
            json $ Object (fromList [("message", "Hello, World!")])
        -- | Test 6: Plain text
        get "plaintext" $
            text "Hello, World!"
