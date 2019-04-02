{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString.Char8
import ServantBench
import Hasql.Connection (settings)
import System.Environment (getArgs)

main :: IO ()
main = do
  [host] <- getArgs
  run 7041 $ dbSettings (pack host)

dbSettings :: ByteString -> ByteString
dbSettings host
  = settings host 5432 "benchmarkdbuser" "benchmarkdbpass" "hello_world"
