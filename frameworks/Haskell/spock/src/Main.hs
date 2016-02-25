{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async      (mapConcurrently)
import           Control.Monad.IO.Class
import           Data.Aeson                    hiding (json)
import           Data.List                     (sort)
import           Data.Maybe                    (catMaybes)
import qualified Database.PostgreSQL.Simple    as PG
import           GHC.Exts
import           Network.HTTP.Types.Status
import           System.Random
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe

import           Models.Fortune
import           Models.World
import           Views.Fortune


dbConn :: PoolOrConn PG.Connection
dbConn =
  PCConn (ConnBuilder
            (PG.connect
                PG.ConnectInfo
                    { PG.connectHost     = "localhost"
                    , PG.connectPort     = 5432
                    , PG.connectUser     = "benchmarkdbuser"
                    , PG.connectPassword = "benchmarkdbpass"
                    , PG.connectDatabase = "hello_world" })
            PG.close
            (PoolCfg 5 5 60))


blaze :: MonadIO m => H.Html -> ActionCtxT ctx m a
blaze h = do
    setHeader "Content-Type" "text/html"
    lazyBytes $ renderHtml h
{-# INLINE blaze #-}


main :: IO ()
main =
    runSpock 3000 $ spock (defaultSpockCfg Nothing dbConn ()) $ do
        -- | Test 1: JSON serialization
        get "json" $
            json $ Object (fromList [("message", "Hello, World!")])
        -- | Test 2: Single database query
        get "db" $ do
            rand <- liftIO $ randomRIO (1, 10000)
            maybeWorld <- runQuery $ fetchWorldById rand
            case maybeWorld of
              Just w  -> json w
              Nothing -> setStatus status404 >> text "World not found."
        -- | Test 3: Multiple database queries
        get "queries" $ do
            queries <- max 1 . min 500 <$> param' "queries"
            let runSQL core = runSpockIO core . runQuery . fetchWorldById =<< randomRIO (1, 10000)
            spockCore <- getSpockHeart
            maybeWorlds <- liftIO $ mapConcurrently (\_ -> runSQL spockCore) ([1..queries] :: [Int])
            json $ catMaybes maybeWorlds
        -- | Test 4: Fortunes
        get "fortune" $ do
            fortunes <- runQuery fetchFortunes
            let newFortune = Fortune 0 "Additional fortune added at request time."
                sortedFortunes = sort (newFortune : fortunes)
            blaze $ renderFortunes sortedFortunes
        -- | Test 5: Database Updates
        -- todo

        -- | Test 6: Plain text
        get "plaintext" $
            text "Hello, World!"
