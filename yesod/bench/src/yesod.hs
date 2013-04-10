{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
import Yesod
import System.Environment (getArgs)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Text (Text)
import Data.Conduit.Pool (Pool)
import Database.Persist.Store (get, PersistValue (PersistInt64))
import Database.Persist.MySQL
import qualified System.Random.MWC as R
import Control.Monad.Primitive (PrimState)
import Control.Monad (replicateM)
import Data.Conduit.Network (bindPort)
import System.Posix.Process (forkProcess)
import Control.Monad (replicateM_)

mkPersist sqlSettings [persist|
World sql=World
    randomNumber Int sql=randomNumber
|]

data App = App
    { appConnPool :: Pool Connection
    , appGen :: R.Gen (PrimState IO)
    }

mkYesod "App" [parseRoutes|
/json JsonR GET
/db DbR GET
/dbs/#Int DbsR GET
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
    Just x <- flip runSqlPool appConnPool $ get (Key $ PersistInt64 i :: WorldId)
    jsonToRepJson $ object ["id" .= i, "randomNumber" .= worldRandomNumber x]

getDbsR :: Int -> Handler RepJson
getDbsR cnt = do
    App {..} <- getYesod
    objs <- replicateM cnt $ do
        i <- liftIO $ R.uniformR (1, 10000) appGen
        Just x <- flip runSqlPool appConnPool $ get (Key $ PersistInt64 i :: WorldId)
        return $ object ["id" .= i, "randomNumber" .= worldRandomNumber x]
    jsonToRepJson $ array objs

main :: IO ()
main = R.withSystemRandom $ \gen -> do
    socket <- bindPort 8000 "*"
    [cores, host] <- getArgs
    pool <- createMySQLPool defaultConnectInfo
        { connectUser = "benchmarkdbuser"
        , connectPassword = "benchmarkdbpass"
        , connectDatabase = "hello_world"
        , connectHost = host
        } 1000
    app <- toWaiAppPlain App
        { appConnPool = pool
        , appGen = gen
        }
    let run = Warp.runSettingsSocket Warp.defaultSettings
                { Warp.settingsPort = 8000
                , Warp.settingsHost = "*"
                , Warp.settingsOnException = const $ return ()
                } socket app
    replicateM_ (read cores - 1) $ forkProcess run
    run
