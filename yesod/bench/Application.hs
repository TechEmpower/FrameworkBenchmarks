{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , makeFoundation
    ) where

import Import
import Control.Monad
import Control.DeepSeq (force)
import System.Random

import qualified Database.Persist.Store
import Database.Persist.Store (PersistValue (..))
import Network.HTTP.Conduit (newManager, def)
import Yesod.Default.Config


import Settings

getJsonR :: Handler RepJson
getJsonR = jsonToRepJson $ object ["message" .= ("Hello, World!" :: Text)]

getDBR :: Handler RepJson
getDBR = do
    i <- liftIO $ randomRIO (1, 10000)
    Just o <- runDB $ get $ Key $ PersistInt64 i
    jsonToRepJson $ object ["id" .= i, "randomNumber" .= worldRandomNumber o]

getDB2R :: Int -> Handler RepJson
getDB2R n = do
    is <- force . take n . randomRs (1, 10000) <$> liftIO newStdGen

    os <- runDB $
        forM is $ \i-> do
            Just o <- get $ Key $ PersistInt64 i
            return $ object ["id" .= i, "randomNumber" .= worldRandomNumber o]

    jsonToRepJson $ array os

mkYesodDispatch "App" resourcesApp

makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = makeFoundation conf >>= toWaiAppPlain

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    dbconf <- withYamlEnvironment "config/mysql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    let foundation = App conf p manager dbconf

    return foundation
