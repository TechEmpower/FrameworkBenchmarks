{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main, resourcesApp, Widget, WorldId) where
import           Control.Monad            (replicateM)
import           Control.Monad.Logger     (runNoLoggingT)
import           Control.Monad.Primitive  (PrimState)
import           Control.Monad.Reader     (ReaderT)
import           Data.Conduit.Pool        (Pool)
import           Data.Int                 (Int64)
import           Data.Text                (Text)
import           Database.MongoDB         (Field ((:=)), (=:))
import qualified Database.MongoDB         as Mongo
import qualified Database.Persist.MongoDB as Mongo
import qualified Database.Persist.MySQL   as My
import           Network                  (PortID (PortNumber))
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment       (getArgs)
import qualified System.Random.MWC        as R
import           Yesod                    hiding (Field)

mkPersist sqlSettings { mpsGeneric = True } [persistLowerCase|
World sql=World
    randomNumber Int sql=randomNumber
#ifdef MONGODB
    id           Int64
    UniqueId
#endif
|]

data App = App
    { appGen      :: !(R.Gen (PrimState IO))
    , mySqlPool   :: !(Pool My.SqlBackend)
    , mongoDBPool :: !(Pool Mongo.Connection)
    }

-- | Not actually using the non-raw mongoDB.
-- persistent-mongoDB expects a field of '_id', not 'id'
mkYesod "App" [parseRoutes|
/json               JsonR     GET

/db                 DbR       GET
/dbs/#Int           DbsR      GET

#ifdef MONGODB
/mongo/db           MongoDbR  GET
/mongo/dbs/#Int     MongoDbsR GET
#endif

/mongo/raw/db       MongoRawDbR  GET
/mongo/raw/dbs/#Int MongoRawDbsR GET
|]

instance Yesod App where
    makeSessionBackend _ = return Nothing
    shouldLog _ _ _ = False
    yesodMiddleware = id

getJsonR :: Handler Value
getJsonR = return $ object ["message" .= ("Hello, World!" :: Text)]


getDbR :: Handler Value
getDbR = getDb (intQuery runMySQL My.toSqlKey)

#ifdef MONGODB
getMongoDbR :: Handler Value
getMongoDbR = getDb (intQuery runMongoDB (getBy . UniqueId))
#endif

getMongoRawDbR :: Handler Value
getMongoRawDbR = getDb rawMongoIntQuery

getDbsR :: Int -> Handler Value
getDbsR cnt = do
    App {..} <- getYesod
    multiRandomHandler (intQuery runMySQL My.toSqlKey) cnt

#ifdef MONGODB
getMongoDbsR :: Int -> Handler Value
getMongoDbsR cnt = multiRandomHandler (intQuery runMongoDB (getBy . UniqueId)) cnt
#endif

getMongoRawDbsR :: Int -> Handler Value
getMongoRawDbsR cnt = multiRandomHandler rawMongoIntQuery cnt


randomNumber :: R.Gen (PrimState IO) -> IO Int64
randomNumber appGen = R.uniformR (1, 10000) appGen

getDb :: (Int64 -> Handler Value) -> Handler Value
getDb query = do
    app <- getYesod
    i <- liftIO (randomNumber (appGen app))
    query i


runMongoDB :: Mongo.Action Handler b -> Handler b
runMongoDB f = do
  App {..} <- getYesod
  Mongo.runMongoDBPoolDef f mongoDBPool

runMySQL :: My.SqlPersistT Handler b -> Handler b
runMySQL f = do
  App {..} <- getYesod
  My.runSqlPool f mySqlPool

intQuery :: (MonadIO m, PersistEntity val, PersistStore backend
            , backend ~ PersistEntityBackend val
            ) =>
           (ReaderT backend m (Maybe val) -> m (Maybe (WorldGeneric backend)))
           -> (Int64 -> Key val)
           -> Int64 -> m Value
intQuery db toKey i = do
    Just x <- db $ get $ toKey i
    return $ jsonResult (worldRandomNumber x)
  where
    jsonResult :: Int -> Value
    jsonResult n = object ["id" .= i, "randomNumber" .= n]

rawMongoIntQuery :: Mongo.Val v => v -> Handler Value
rawMongoIntQuery i = do
    Just x <- runMongoDB $ Mongo.findOne (Mongo.select ["id" =: i] "World")
    return $ documentToJson x

multiRandomHandler :: ToJSON a
                   => (Int64 -> Handler a)
                   -> Int
                   -> Handler Value
multiRandomHandler operation cnt = do
    App {..} <- getYesod
    nums <- liftIO $ replicateM cnt (randomNumber appGen)
    return . array =<< mapM operation nums

documentToJson :: [Field] -> Value
documentToJson = object . map toAssoc
  where
    toAssoc :: Field -> (Text, Value)
    toAssoc ("_id" := v) = ("id", toJSON v)
    toAssoc (l := v) = (l, toJSON v)

instance ToJSON Mongo.Value where
  toJSON (Mongo.Int32 i)  = toJSON i
  toJSON (Mongo.Int64 i)  = toJSON i
  toJSON (Mongo.Float f)  = toJSON f
  toJSON (Mongo.Doc d)   = documentToJson d
  toJSON s = error $ "no convert for: " ++ show s



main :: IO ()
main = R.withSystemRandom $ \gen -> do
    [_cores, host] <- getArgs
    myPool <- runNoLoggingT $ My.createMySQLPool My.defaultConnectInfo
        { My.connectUser = "benchmarkdbuser"
        , My.connectPassword = "benchmarkdbpass"
        , My.connectDatabase = "hello_world"
        , My.connectHost = host
        } 1000

    mongoPool <- Mongo.createMongoDBPool "hello_world" host (PortNumber 27017)
        (Just (Mongo.MongoAuth "benchmarkdbuser" "benchmarkdbpass"))
           1  -- what is the optimal stripe count? 1 is said to be a good default
           1000
           3  -- 3 second timeout

    app <- toWaiAppPlain App
        { appGen = gen
        , mySqlPool = myPool
        , mongoDBPool = mongoPool
        }
    Warp.runSettings
        ( Warp.setPort 8000
        $ Warp.setHost "*"
        $ Warp.setOnException (\_ _ -> return ())
          Warp.defaultSettings
        ) app
