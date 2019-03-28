{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Lib
import qualified GHC.Conc
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  dbHost <- case args of
    [x] -> pure x
    _ -> pure "0.0.0.0"
    -- _ -> fail "Usage: warp-postgres-wire <DATABASE_HOST>"
  numCaps <- GHC.Conc.getNumCapabilities
  Lib.main $ Lib.Config {
    Lib.configHost    = dbHost,
    Lib.configName    = "hello_world",
    Lib.configUser    = "benchmarkdbuser",
    Lib.configPass    = "benchmarkdbpass",
    Lib.configStripes = numCaps,
    Lib.configPoolSize= 512
  }
