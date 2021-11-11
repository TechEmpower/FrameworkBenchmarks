{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Control.Concurrent (getNumCapabilities)
import           System.Environment (lookupEnv)

import qualified Lib

main :: IO ()
main = do
  testName <- lookupEnv "TFB_TEST_NAME"
  putStrLn $ "Test is: " ++ show testName
  capabilities <- getNumCapabilities
  Lib.main capabilities
