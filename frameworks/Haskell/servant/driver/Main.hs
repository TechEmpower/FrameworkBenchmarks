{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString
import ServantBench
import Hasql.Connection (settings)

main :: IO ()
main = run 7041 dbSettings

dbSettings :: ByteString
dbSettings
  = settings "localhost" 5432 "benchmarkdbuser" "benchmarkdbpass" "hello_world"
