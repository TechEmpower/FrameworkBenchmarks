{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
import Yesod hiding (Field)
import System.Environment (getArgs)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Text (Text)
import Data.Conduit.Pool (Pool)
import Database.Persist.Store (PersistValue (PersistInt64))
import qualified Database.Persist.MySQL as My
import qualified Database.Persist.MongoDB as Mongo
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:), Field((:=)))
import qualified System.Random.MWC as R
import Control.Monad.Primitive (PrimState)
import Control.Monad (replicateM)
import Data.Conduit.Network (bindPort)
import System.Posix.Process (forkProcess)
import Control.Monad (replicateM_)
import Network (PortID (PortNumber))
import Control.Concurrent.Async.Lifted (mapConcurrently)
import Data.Int (Int64)
import Data.Aeson (ToJSON(..))

mkPersist sqlSettings [persist|
World sql=World
    randomNumber Int sql=randomNumber
|]

data App = App
    { appGen :: R.Gen (PrimState IO)
    , mySqlPool :: Pool My.Connection
    , mongoDBPool :: Pool Mongo.Connection
    }

-- | Not actually using the non-raw mongoDB.
-- persistent-mongoDB expects a field of '_id', not 'id'
mkYesod "App" [parseRoutes|
/json               JsonR     GET

/db                 DbR       GET
/dbs/#Int           DbsR      GET

/mongo/db           MongoDbR  GET
/mongo/dbs/#Int     MongoDbsR GET

/mongo/raw/db       MongoRawDbR  GET
/mongo/raw/dbs/#Int MongoRawDbsR GET
|]

instance Yesod App where
    makeSessionBackend _ = return Nothing
    shouldLog _ _ _ = False
    yesodMiddleware = id

getJsonR :: Handler RepJson
getJsonR = jsonToRepJson $ object ["message" .= ("Hello, World!" :: Text)]


getDbR :: Handler RepJson
getDbR = getDb (intQuery runMySQL )

getMongoDbR :: Handler RepJson
getMongoDbR = getDb (intQuery runMongoDB )

getMongoRawDbR :: Handler RepJson
getMongoRawDbR = getDb rawMongoIntQuery

getDbsR :: Int -> Handler RepJson
getDbsR cnt = do
    App {..} <- getYesod
    multiRandomHandler (intQuery runMySQL) cnt

getMongoDbsR :: Int -> Handler RepJson
getMongoDbsR cnt = multiRandomHandler (intQuery runMongoDB) cnt

getMongoRawDbsR :: Int -> Handler RepJson
getMongoRawDbsR cnt = multiRandomHandler rawMongoIntQuery cnt


randomNumber :: R.Gen (PrimState IO) -> IO Int64
randomNumber appGen = R.uniformR (1, 10000) appGen

getDb :: ToJSON a => (Int64 -> Handler a) -> Handler RepJson
getDb query = do
    app <- getYesod
    i <- liftIO (randomNumber (appGen app))
    jsonToRepJson =<< query i


runMongoDB :: Mongo.Action Handler b -> Handler b
runMongoDB f = do
  App {..} <- getYesod
  Mongo.runMongoDBPoolDef f mongoDBPool

runMySQL :: My.SqlPersist Handler b -> Handler b
runMySQL f = do
  App {..} <- getYesod
  My.runSqlPool f mySqlPool

intQuery :: forall (m :: * -> *) (m1 :: * -> *) val backend.
           (Monad m, PersistEntity val, PersistStore m1,
            PersistEntityBackend val ~ PersistMonadBackend m1) =>
           (m1 (Maybe val) -> m (Maybe (WorldGeneric backend)))
           -> Int64 -> m Value
intQuery db i = do
    Just x <- db $ get (Key $ PersistInt64 i)
    return $ jsonResult i (worldRandomNumber x)
  where
    jsonResult :: Int64 -> Int -> Value
    jsonResult i n = object ["id" .= i, "randomNumber" .= n]

rawMongoIntQuery :: Mongo.Val v => v -> Handler Value
rawMongoIntQuery i = do
    Just x <- runMongoDB $ Mongo.findOne (Mongo.select ["id" =: i] "World")
    return $ documentToJson x

multiRandomHandler :: ToJSON a
                   => (Int64 -> Handler a)
                   -> Int
                   -> Handler RepJson
multiRandomHandler operation cnt = do
    App {..} <- getYesod
    nums <- liftIO $ replicateM cnt (randomNumber appGen)
    jsonToRepJson . array =<< mapConcurrently operation nums

documentToJson :: [Field] -> Value
documentToJson = object . map toAssoc
  where
    toAssoc :: Field -> (Text, Value)
    toAssoc ("_id" := v) = ("id", toJSON v)
    toAssoc (l := v) = (l, toJSON v)

instance ToJSON Mongo.Value where
  toJSON (Mongo.Int32 i)  = toJSON i
  toJSON (Mongo.Int64 i)  = toJSON i
  toJSON (Mongo.Doc d)   = documentToJson d
  toJSON s = error $ "no convert for: " ++ show s



main :: IO ()
main = R.withSystemRandom $ \gen -> do
    socket <- bindPort 8000 "*"
    [cores, host] <- getArgs
    myPool <- My.createMySQLPool My.defaultConnectInfo
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
    let run = Warp.runSettingsSocket Warp.defaultSettings
                { Warp.settingsPort = 8000
                , Warp.settingsHost = "*"
                , Warp.settingsOnException = const $ return ()
                } socket app
    replicateM_ (read cores - 1) $ forkProcess run
    run
