{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Configurator
import Data.Int
import Data.Text                                 (Text)
import Data.Pool
import Database.MySQL.Simple
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.QueryResults
import Prelude                            hiding (lookup)
import Snap.Core
import Snap.Http.Server
import System.Random

import qualified Data.ByteString.Char8 as B

data RandQuery = RQ !Int !Int

instance ToJSON RandQuery where
    toJSON (RQ i n) = object ["id" .= i, "randomNumber" .= n]

instance QueryResults RandQuery where
    convertResults [fa, fb] [va, vb] = RQ a b
        where
          !a = convert fa va
          !b = convert fb vb
    convertResults fs vs = convertError fs vs 2

main :: IO ()
main = do
    db <- load [Required "cfg/db.cfg"]
    foos <- mapM (lookup db) ["host", "uname", "pword", "dbase", "dport"]
    let foos' = sequence foos
    maybe (putStrLn "No foo") dbSetup foos'

dbSetup :: [String] -> IO ()
dbSetup sets = do
    pool <- createPool (connect $ getConnInfo sets) close 1 10 50
    httpServe config $ site pool

config :: Config Snap a
config = setAccessLog ConfigNoLog
    . setErrorLog ConfigNoLog
    . setPort 8000
    $ defaultConfig

getConnInfo :: [String] -> ConnectInfo
getConnInfo [host, user, pwd, db, port] = defaultConnectInfo
    { connectHost     = host
    , connectUser     = user
    , connectPassword = pwd
    , connectDatabase = db
    , connectPort     = read port
    }
getConnInfo _ = defaultConnectInfo

site :: Pool Connection -> Snap ()
site pool = route
    [ ("json",      jsonHandler)
    , ("db",        dbHandler pool)
    , ("plaintext", writeBS "Hello, World!")
    ]

jsonHandler :: Snap ()
jsonHandler = do
    modifyResponse (setContentType "application/json")
    writeLBS $ encode [ "message" .= ("Hello, World!" :: Text) ]

dbHandler :: Pool Connection -> Snap ()
dbHandler pool = do
    modifyResponse (setContentType "application/json")
    qs <- getQueryParam "queries"
    runAll pool $ maybe 1 fst (qs >>= B.readInt)

runAll :: Pool Connection -> Int -> Snap ()
runAll pool i = do
    !rs <- take i . randomRs (1, 10000) <$> liftIO newStdGen
    qry <- liftIO $ withResource pool (forM rs . runOne)
    writeLBS $ encode qry

runOne :: Connection -> Int -> IO RandQuery
runOne conn = fmap head . query conn "SELECT * FROM World where id=?" . Only
