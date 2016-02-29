{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Aeson                    hiding (json)
import           Data.List                     (sort)
import qualified Database.PostgreSQL.Simple    as PG
import           GHC.Exts
import           Network.HTTP.Types.Status
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5              as H
import           Web.Spock.Safe

import           Models.Fortune
import           Models.World
import           Views.Fortune


creds :: PG.ConnectInfo
creds =
    PG.ConnectInfo
        { PG.connectHost     = "localhost"
        , PG.connectPort     = 5432
        , PG.connectUser     = "benchmarkdbuser"
        , PG.connectPassword = "benchmarkdbpass"
        , PG.connectDatabase = "hello_world" }


dbConn :: PoolOrConn PG.Connection
dbConn = PCConn (ConnBuilder (PG.connect creds) PG.close (PoolCfg 5 5 60))


blaze :: MonadIO m => H.Html -> ActionCtxT ctx m a
blaze h = do
    setHeader "Content-Type" "text/html"
    lazyBytes $ renderHtml h
{-# INLINE blaze #-}


-- | Test 1: JSON serialization
test1 :: MonadIO m => ActionCtxT ctx m a
test1 = do
    setHeader "Content-Type" "application/json"
    lazyBytes $ encode $ Object (fromList [("message", "Hello, World!")])
{-# INLINE test1 #-}

-- | Test 2: Single database query
test2 :: ActionCtxT ctx (WebStateM PG.Connection b ()) a
test2 = do
    maybeWorld <- runQuery getRandomWorld
    setHeader "Content-Type" "application/json"
    case maybeWorld of
      Just w  -> lazyBytes $ encode w
      Nothing -> setStatus status404 >> lazyBytes "{\"error\": \"World not found.\"}"
{-# INLINE test2 #-}

-- | Test 3: Multiple database queries
test3 :: ActionCtxT ctx (WebStateM PG.Connection b ()) a
test3 = do
    queries <- max 1 . min 500 <$> param' "queries"
    worlds <- runQuery $ fetchRandomWorldsAsync queries
    setHeader "Content-Type" "application/json"
    lazyBytes $ encode worlds
{-# INLINE test3 #-}

-- | Test 4: Fortunes
test4 :: ActionCtxT ctx (WebStateM PG.Connection b ()) a
test4 = do
    fortunes <- runQuery fetchFortunes
    blaze $ renderFortunes $ sort (newFortune : fortunes)
    where
      newFortune = Fortune 0 "Additional fortune added at request time."
{-# INLINE test4 #-}

-- | Test 5: Database Updates
test5 :: ActionCtxT ctx (WebStateM PG.Connection b ()) a
test5 = do
    queries <- max 1 . min 500 <$> param' "queries"
    worlds <- runQuery $ fetchRandomWorldsAsync queries
    updatedWorlds <- runQuery $ updateWorldsRandomAsync worlds
    setHeader "Content-Type" "application/json"
    lazyBytes $ encode updatedWorlds
{-# INLINE test5 #-}

-- | Test 6: Plain text
test6 :: MonadIO m => ActionCtxT ctx m a
test6 = do
    setHeader "Content-Type" "text/plain"
    lazyBytes "Hello, World!"
{-# INLINE test6 #-}


main :: IO ()
main =
    runSpock 3000 $ spock (defaultSpockCfg Nothing dbConn ()) $ do
        get "json"      test1
        get "db"        test2
        get "queries"   test3
        get "fortune"   test4
        get "updates"   test5
        get "plaintext" test6
