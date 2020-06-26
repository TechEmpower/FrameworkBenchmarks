{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Lib
import qualified GHC.Conc
import           System.Environment (getArgs, lookupEnv)

main :: IO ()
main = do
  testName <- lookupEnv "TFB_TEST_NAME"
  putStrLn $ "Test is: " ++ show testName
  args <- getArgs
  dbHost <- case args of
    [x] -> pure x
    _ -> pure "0.0.0.0"
  numCaps <- GHC.Conc.getNumCapabilities
  Lib.main $ Lib.Config {
    Lib.configHost    = dbHost,
    Lib.configName    = "hello_world",
    Lib.configUser    = "benchmarkdbuser",
    Lib.configPass    = "benchmarkdbpass",
    Lib.configStripes = numCaps,
    Lib.configPoolSize= 512
  }
