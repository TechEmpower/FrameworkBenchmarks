{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Lib (
    main
  , Db.Config(..)
) where

import qualified TFB.Types as Types
import qualified TFB.Db as Db
import           MIME (Plain, HTML, Json)
import qualified Data.Either as Either
import           Data.List (sortOn)
import           Control.Monad (replicateM)

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.BufferBuilder.Json    as Json
import           Data.BufferBuilder.Json    ((.=))
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Html
import           Html                       ((#))
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant
import qualified System.Random.MWC          as MWC

-- * API contracts

type API =
       "json"       :> Get '[Json] Json.ObjectBuilder
  :<|> "db"         :> Get '[Json] Types.World
  :<|> "queries"    :> QueryParam "queries" Types.Count :> Get '[Json] [Types.World]
  :<|> "fortune"    :> Get '[HTML] Types.FortunesHtml
  :<|> "updates"    :> QueryParam "queries" Types.Count :> Get '[Json] [Types.World]
  :<|> "plaintext"  :> Get '[Plain] ByteString

api :: Proxy API
api = Proxy

server :: MWC.GenIO -> Db.Pool -> Server API
server gen dbPool =
      getJson
 :<|> getWorld gen dbPool
 :<|> getWorlds gen dbPool
 :<|> getFortunes dbPool
 :<|> updateWorlds gen dbPool
 :<|> plaintext

-- | entry point
main :: Db.Config -> IO ()
main dbConfig = do
  putStrLn "Config is:"
  print dbConfig
  putStrLn "Initializing database connection pool..."
  dbPool <- Db.mkPool dbConfig
  putStrLn "Initializing PRNG seed..."
  gen <- MWC.create
  putStrLn "Servant is ready to serve you"
  Warp.run 7041 $ serve api $ server gen dbPool

------------------------------------------------------------------------------

-- * Test 1: JSON serialization

getJson :: Handler Json.ObjectBuilder
getJson = return $ "message" .= Types.unsafeJsonString "Hello, World!"
{-# INLINE getJson #-}

-- * Test 2: Single database query

getWorld :: MWC.GenIO -> Db.Pool -> Handler Types.World
getWorld gen dbPool = do
  wId <- liftIO $ randomId gen
  res <- liftIO $ Db.queryWorldById dbPool wId
  Either.either respondDbError pure $ res
{-# INLINE getWorld #-}

-- * Test 3: Multiple database query

getWorlds :: MWC.GenIO -> Db.Pool -> Maybe Types.Count -> Handler [Types.World]
getWorlds gen dbPool mCount = do
  wIds <- liftIO $ replicateM count $ randomId gen
  res <- liftIO $ Db.queryWorldByIds dbPool wIds
  Either.either respondDbError pure $ res
  where
    count = Types.getCount mCount
{-# INLINE getWorlds #-}

-- * Test 4: Fortunes

getFortunes :: Db.Pool -> Handler Types.FortunesHtml
getFortunes dbPool = do
  res <- liftIO $ Db.queryFortunes dbPool
  case res of
    Left e -> respondDbError e
    Right fs -> return $ do
      let new = Types.Fortune 0 "Additional fortune added at request time."
      let header = Html.tr_ $ Html.th_ (Html.Raw "id") # Html.th_ (Html.Raw "message")
      let mkRow f = Html.tr_ $ Html.td_ (fromIntegral $ Types.fId f) # Html.td_ (Types.fMessage $ f)
      let rows = fmap mkRow $ sortOn Types.fMessage (new : fs)
      Html.doctype_ #
        Html.html_ (
          Html.head_ (
            Html.title_ (Html.Raw "Fortunes")
          ) #
          Html.body_ ( Html.table_ $
            header # rows
          )
        )
{-# INLINE getFortunes #-}

-- * Test 5: Updates

updateWorlds :: MWC.GenIO -> Db.Pool -> Maybe Types.Count -> Handler [Types.World]
updateWorlds gen dbPool mCount = do
  wIds <- liftIO $ replicateM count $ randomId gen
  res <- liftIO $ Db.queryWorldByIds dbPool wIds
  Either.either respondDbError (go dbPool) res
  where
    count = Types.getCount mCount
    go conn ws = do
      wNumbers <- liftIO $ replicateM count $ randomId gen
      wsUp <- liftIO $ Db.updateWorlds conn . zip ws $ fmap fromIntegral wNumbers
      Either.either respondDbError pure wsUp
{-# INLINE updateWorlds #-}

-- * Test 6: Plaintext endpoint

plaintext :: Handler ByteString
plaintext = return "Hello, World!"
{-# INLINE plaintext #-}

------------------------------------------------------------------------------

-- * utils

respondDbError :: Db.Error -> Handler a
respondDbError e = throwError err500 { errBody = LBSC.pack . show $ e }

randomId :: MWC.GenIO -> IO Types.QId
randomId = MWC.uniformR (1, 10000)
