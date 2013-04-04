{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Database.HDBC
import Database.HDBC.MySQL
import Data.Configurator
import Data.Int
import Data.Pool
import Paths_snap_bench
import Prelude hiding (lookup)
import qualified Data.ByteString.Char8 as B
import Snap.Core
import Snap.Http.Server
import System.Random
import Text.JSON

main :: IO ()
main = do
    fp <- getDataFileName "db.cfg"
    db <- load [Required fp]
    foos <- mapM (lookup db) ["host", "uname", "pword", "dbase", "dport"]
    let foos' = sequence foos
    maybe (putStrLn "No foo") dbSetup foos'

dbSetup :: [String] -> IO ()
dbSetup sets = let info = getConnInfo sets
                   config = setAccessLog ConfigNoLog $
                            setErrorLog ConfigNoLog $
                            setPort 8000 $
                            defaultConfig
                in do pool <- createPool (connectMySQL info) disconnect 1 10 50
                      httpServe config $ site pool

getConnInfo :: [String] -> MySQLConnectInfo
getConnInfo [host,uname,pword,dbase,dport] = defaultMySQLConnectInfo {
    mysqlHost     = host,
    mysqlUser     = uname,
    mysqlPassword = pword,
    mysqlDatabase = dbase,
    mysqlPort     = read dport
    }
getConnInfo _ = defaultMySQLConnectInfo

site :: IConnection a => Pool a -> Snap ()
site pool = route [ ("json", jsonHandler)
                  , ("db", dbHandler pool)
                  ]

jsonHandler :: Snap ()
jsonHandler = do
    modifyResponse (setContentType "application/json")
    writeBS $ B.pack $ encode $ toJSObject [("message", "Hello, World!" :: String)]

dbHandler :: IConnection a => Pool a -> Snap ()
dbHandler pool = do
    modifyResponse (setContentType "application/json")
    queries <- getQueryParam "queries"
    maybe (db 1) fn (gn queries)
    where fn q = db q
          gn s = fmap fst $ s >>= B.readInt
          db = dbHandler' pool

dbHandler' :: IConnection a => Pool a -> Int -> Snap ()
dbHandler' pool i = do
    rows <- liftIO $ withResource pool runQuery
    writeBS $ B.pack $ encode $ map jsonRow $ concat rows
    where runQuery conn = replicateM i $ do
              (ix,_) <- randomR (1, 10000 :: Int32) <$> newStdGen
              withSB $ quickQuery' conn query [SqlInt32 ix]
          withSB = withRTSSignalsBlocked
          query = "SELECT * FROM World where id=?"

jsonRow :: [SqlValue] -> JSValue
jsonRow [i, v] = JSObject $ toJSObject [("id", showJSON i), ("randomNumber", showJSON v)]
jsonRow _ = JSNull

instance JSON SqlValue where
    readJSON = undefined -- Poor form, but unneeded
    showJSON v = case v of -- We're just doing the obvious stuff since this is a 1-off
        SqlString s -> JSString $ toJSString s
        SqlByteString s -> showJSON s
        SqlWord32 i -> showJSON i
        SqlWord64 i -> showJSON i
        SqlInt32 i -> showJSON i
        SqlInt64 i -> showJSON i
        SqlInteger i -> showJSON i
        SqlChar c -> showJSON c
        SqlBool b -> showJSON b
        SqlDouble d -> showJSON d
        _ -> JSNull
