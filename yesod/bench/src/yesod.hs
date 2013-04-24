{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
import Yesod
import System.Environment (getArgs)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Text (Text)
import Data.Conduit.Pool (Pool)
import Database.Persist.Store (PersistValue (PersistInt64))
import qualified Database.Persist.MySQL as My
import qualified Database.Persist.MongoDB as Mongo
import qualified System.Random.MWC as R
import Control.Monad.Primitive (PrimState)
import Control.Monad (replicateM)
import Data.Conduit.Network (bindPort)
import System.Posix.Process (forkProcess)
import Control.Monad (replicateM_)
import Network (PortID (PortNumber))
import Control.Concurrent.Async.Lifted (mapConcurrently)
import Data.Int (Int64)

mkPersist sqlSettings [persist|
World sql=World
    randomNumber Int sql=randomNumber
|]

data App = App
    { appGen :: R.Gen (PrimState IO)
    , mySqlPool :: Pool My.Connection
    , mongoDBPool :: Pool Mongo.Connection
    }

mkYesod "App" [parseRoutes|
/json           JsonR     GET

/db             DbR       GET
/dbs/#Int       DbsR      GET

/mongo/db       MongoDbR  GET
/mongo/dbs/#Int MongoDbsR GET
|]

instance Yesod App where
    makeSessionBackend _ = return Nothing
    shouldLog _ _ _ = False
    yesodMiddleware = id

getJsonR :: Handler RepJson
getJsonR = jsonToRepJson $ object ["message" .= ("Hello, World!" :: Text)]

getDbR :: Handler RepJson
getDbR = do
    App {..} <- getYesod
    i <- liftIO $ R.uniformR (1, 10000) appGen
    jsonToRepJson =<< intQuery (flip My.runSqlPool mySqlPool) i

getDbsR :: Int -> Handler RepJson
getDbsR cnt = do
  App {..} <- getYesod
  multiRandomHandler (flip My.runSqlPool mySqlPool) appGen cnt

getMongoDbR :: Handler RepJson
getMongoDbR = do
    App {..} <- getYesod
    i <- liftIO $ R.uniformR (1, 10000) appGen
    jsonToRepJson =<< intQuery (flip Mongo.runMongoDBPoolDef mongoDBPool) i

getMongoDbsR :: Int -> Handler RepJson
getMongoDbsR cnt = do
  App {..} <- getYesod
  multiRandomHandler (flip Mongo.runMongoDBPoolDef mongoDBPool) appGen cnt

multiRandomHandler :: forall (m :: * -> *) backend.
    (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend (WorldGeneric backend))
  => (m (Maybe (WorldGeneric backend))
       -> Handler (Maybe (WorldGeneric backend)))
  -> R.Gen (PrimState IO)
  -> Int
  -> Handler RepJson
multiRandomHandler db appGen cnt = do
    nums <- liftIO $ replicateM cnt $ R.uniformR (1, 10000) appGen
    jsonToRepJson . array =<< mapConcurrently (intQuery db) nums

intQuery :: forall (m :: * -> *) backend.
    (PersistStore m, PersistMonadBackend m ~ PersistEntityBackend (WorldGeneric backend))
  => (m (Maybe (WorldGeneric backend))
       -> Handler (Maybe (WorldGeneric backend)))
  -> Int64
  -> Handler Value
intQuery db i = do
    Just x <- db $ get (Key $ PersistInt64 i)
    return $ object ["id" .= i, "randomNumber" .= worldRandomNumber x]

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
