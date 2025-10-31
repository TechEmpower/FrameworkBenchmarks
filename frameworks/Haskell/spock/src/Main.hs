{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified GHC.Conc
import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import           Data.Aeson                    hiding (json)
import           Data.List                     (sort)
import           Data.Maybe                    (catMaybes, fromMaybe)
import           Data.Pool                     (Pool)
import qualified Data.Pool                     as Pool
import qualified Database.PostgreSQL.Simple    as PG
import           GHC.Exts
import           Network.HTTP.Types.Status
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5              as H
import           Web.Spock

import           Models.Fortune
import           Models.World
import           Views.Fortune
import Web.Spock.Config


poolCfg :: Int -> PoolCfg
poolCfg numStripes = PoolCfg
    { pc_stripes = numStripes
    , pc_resPerStripe = 20
    , pc_keepOpenTime = 20
    }


mkPool :: PoolCfg -> IO (Pool PG.Connection)
mkPool cfg = Pool.createPool
            dbConnect
            PG.close
            (pc_stripes cfg)
            (pc_keepOpenTime cfg)
            (pc_resPerStripe cfg)

dbConnect :: IO PG.Connection
dbConnect = PG.connect creds
  where
    creds =
        PG.ConnectInfo
            { PG.connectHost     = "tfb-database"
            , PG.connectPort     = 5432
            , PG.connectUser     = "benchmarkdbuser"
            , PG.connectPassword = "benchmarkdbpass"
            , PG.connectDatabase = "hello_world"
            }


blaze :: MonadIO m => H.Html -> ActionCtxT ctx m a
blaze h = do
    setHeader "Content-Type" "text/html; charset=UTF-8"
    lazyBytes $ renderHtml h
{-# INLINE blaze #-}


getQueriesNumber :: MonadIO m => ActionCtxT ctx m Int
getQueriesNumber = do
    queriesM <- param "queries"
    return $ max 1 . min 500 $ fromMaybe 1 queriesM
{-# INLINE getQueriesNumber #-}


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
test3 :: Pool PG.Connection -> ActionCtxT ctx (WebStateM PG.Connection b ()) a
test3 pool = do
    queries <- getQueriesNumber
    worlds <- liftIO $ mapConcurrently (const (Pool.withResource pool getRandomWorld)) [1..queries]
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
test5 :: Pool PG.Connection -> ActionCtxT ctx (WebStateM PG.Connection b ()) a
test5 pool = do
    queries <- getQueriesNumber
    worlds <- liftIO $ mapConcurrently (const (Pool.withResource pool getRandomWorld)) [1..queries]
    updatedWorlds <- liftIO $ mapConcurrently (Pool.withResource pool . updateWorldRandom) (catMaybes worlds)
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
main = do
    numCaps <- GHC.Conc.getNumCapabilities
    let numStripes = max 1 numCaps
    pool <- mkPool (poolCfg numStripes)
    spockCfg <- defaultSpockCfg () (PCPool pool) ()
    runSpock 3000 $ spock spockCfg $ do
        get "json"        test1
        get "db"          test2
        get "queries"   $ test3 pool
        get "fortunes"     test4
        get "updates"   $ test5 pool
        get "plaintext"   test6
