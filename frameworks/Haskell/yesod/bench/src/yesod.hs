{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main, resourcesApp, Widget, WorldId) where
import           Blaze.ByteString.Builder
import           Control.Concurrent           (runInUnboundThread)
import           Control.Monad                (replicateM)
import           Control.Monad.Logger         (runNoLoggingT)
import           Control.Monad.Primitive      (PrimState)
import           Control.Monad.Reader         (ReaderT)
import           Control.Monad.Trans.Resource (InternalState)
import           Data.Aeson                   (encode)
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit.Pool            (Pool, createPool)
import           Data.Int                     (Int64)
import           Data.IORef                   (newIORef)
import           Data.Pool                    (withResource)
import           Data.Text                    (Text)
import           Database.MongoDB             (Field ((:=)), (=:))
import qualified Database.MongoDB             as Mongo
import           Database.Persist             (Key, PersistEntity,
                                               PersistEntityBackend,
                                               PersistStore, get)
import qualified Database.Persist.MySQL       as My
import           Database.Persist.TH          (mkPersist, mpsGeneric,
                                               persistLowerCase, sqlSettings)
import           Network                      (PortID (PortNumber))
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp     as Warp
import           System.Environment           (getArgs)
import           System.IO.Unsafe             (unsafePerformIO)
import qualified System.Random.MWC            as R
import           Yesod.Core

mkPersist sqlSettings { mpsGeneric = True } [persistLowerCase|
World sql=World
    randomNumber Int sql=randomNumber
|]

data App = App
    { appGen      :: !(R.Gen (PrimState IO))
    , mySqlPool   :: !(Pool My.SqlBackend)
    , mongoDBPool :: !(Pool Mongo.Pipe)
    }

-- | Not actually using the non-raw mongoDB.
-- persistent-mongoDB expects a field of '_id', not 'id'
mkYesod "App" [parseRoutes|
/json               JsonR     GET

/db                 DbR       GET
/dbs/#Int           DbsR      GET

/mongo/raw/db       MongoRawDbR  GET
/mongo/raw/dbs/#Int MongoRawDbsR GET
|]

fakeInternalState :: InternalState
fakeInternalState = unsafePerformIO $ newIORef $ error "fakeInternalState forced"
{-# NOINLINE fakeInternalState #-}

instance Yesod App where
    makeSessionBackend _ = return Nothing
    {-# INLINE makeSessionBackend #-}
    shouldLog _ _ _ = False
    {-# INLINE shouldLog #-}
    yesodMiddleware = id
    {-# INLINE yesodMiddleware #-}
    cleanPath _ = Right
    {-# INLINE cleanPath #-}
    yesodWithInternalState _ _ = ($ fakeInternalState)
    {-# INLINE yesodWithInternalState #-}
    maximumContentLength _ _ = Nothing
    {-# INLINE maximumContentLength #-}

getJsonR :: Handler ()
getJsonR = sendWaiResponse
         $ responseBuilder
            status200
            [("Content-Type", typeJson)]
         $ copyByteString
         $ L.toStrict
         $ encode
         $ object ["message" .= ("Hello, World!" :: Text)]


getDbR :: Handler Value
getDbR = getDb (intQuery runMySQL My.toSqlKey)

getMongoRawDbR :: Handler Value
getMongoRawDbR = getDb rawMongoIntQuery

getDbsR :: Int -> Handler Value
getDbsR cnt = do
    App {..} <- getYesod
    multiRandomHandler (intQuery runMySQL My.toSqlKey) cnt

getMongoRawDbsR :: Int -> Handler Value
getMongoRawDbsR cnt = multiRandomHandler rawMongoIntQuery cnt


randomNumber :: R.Gen (PrimState IO) -> IO Int64
randomNumber appGen = R.uniformR (1, 10000) appGen

getDb :: (Int64 -> Handler Value) -> Handler Value
getDb query = do
    app <- getYesod
    i <- liftIO (randomNumber (appGen app))
    value <- query i
    sendWaiResponse
        $ responseBuilder
            status200
            [("Content-Type", typeJson)]
        $ copyByteString
        $ L.toStrict
        $ encode value


runMongoDB :: Mongo.Action Handler b -> Handler b
runMongoDB f = do
  App {..} <- getYesod
  withResource mongoDBPool $ \pipe ->
    Mongo.access pipe Mongo.ReadStaleOk "hello_world" f

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
    [cores, host] <- getArgs
    myPool <- runNoLoggingT $ My.createMySQLPool My.defaultConnectInfo
        { My.connectUser = "benchmarkdbuser"
        , My.connectPassword = "benchmarkdbpass"
        , My.connectDatabase = "hello_world"
        , My.connectHost = host
        } 1000

    mongoPool <- createPool
        (Mongo.connect $ Mongo.Host host $ PortNumber 27017)
        Mongo.close
        (read cores) -- what is the optimal stripe count? 1 is said to be a good default
        3  -- 3 second timeout
        1000

    app <- toWaiAppPlain App
        { appGen = gen
        , mySqlPool = myPool
        , mongoDBPool = mongoPool
        }
    runInUnboundThread $ Warp.runSettings
        ( Warp.setPort 8000
        $ Warp.setHost "*"
        $ Warp.setOnException (\_ _ -> return ())
          Warp.defaultSettings
        ) app
