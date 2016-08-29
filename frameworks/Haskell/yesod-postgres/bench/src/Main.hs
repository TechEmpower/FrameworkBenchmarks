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
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main, resourcesApp, Widget, WorldId) where
import           Blaze.ByteString.Builder
import           Control.Applicative           (liftA2)
import           Control.Concurrent            (runInUnboundThread)
import           Control.Monad                 (replicateM, forM)
import           Control.Monad.Logger          (runNoLoggingT)
import           Control.Monad.Primitive       (PrimState)
import           Control.Monad.Reader          (ReaderT)
import           Control.Monad.Trans.Resource  (InternalState)
import           Data.Aeson                    (encode)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Pool                     (Pool, createPool)
import           Data.Int                      (Int64)
import           Data.IORef                    (newIORef)
import           Data.Function                 (on)
import           Data.List                     (sortBy)
import           Data.Pool                     (withResource)
import           Data.Text                     (Text)
import           Database.Persist              (Key, PersistEntity,
                                                PersistEntityBackend,
                                                PersistStore, get, update,
                                                (=.))
import qualified Database.Persist.Postgresql    as Pg
import Database.Persist.Sql
import           Database.Persist.TH           (mkPersist, mpsGeneric,
                                                persistLowerCase, sqlSettings)
import           Network                       (PortID (PortNumber))
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           System.Environment            (getArgs)
import           System.IO.Unsafe              (unsafePerformIO)
import qualified System.Random.MWC             as R
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Blaze.Html
import           Yesod
import           Data.Text.Read
import Data.Maybe (fromJust)

mkPersist sqlSettings { mpsGeneric = True } [persistLowerCase|
World sql=World
    randomNumber Int sql=randomnumber
|]

mkPersist sqlSettings { mpsGeneric = True } [persistLowerCase|
Fortune sql=Fortune
    message Text sql=message
|]

instance ToJSON (Entity World) where
  toJSON (Entity wId wRow) = object [
    "id" .= wId
    ,"randomNumber" .= (worldRandomNumber wRow)
    ]

instance ToMarkup FortuneId where
  toMarkup = toMarkup . fromSqlKey

data App = App
    { appGen      :: !(R.Gen (PrimState IO))
    , appDbPool   :: !(Pool Pg.SqlBackend)
    }

-- | Not actually using the non-raw mongoDB.
-- persistent-mongoDB expects a field of '_id', not 'id'
-- mkYesod "App" [parseRoutes|
-- /json               JsonR     GET

-- /db                 DbR       GET
-- /dbs/#Int           DbsR      GET
-- !/dbs/#Text         DbsDefaultR  GET

-- /mongo/raw/db       MongoRawDbR  GET
-- /mongo/raw/dbs/#Int MongoRawDbsR GET
-- !/mongo/raw/dbs/#Text MongoRawDbsDefaultR GET

-- /updates/#Int       UpdatesR     GET
-- !/updates/#Text     UpdatesDefaultR GET

-- /fortunes           FortunesR    GET

-- /plaintext          PlaintextR   GET
-- |]


mkYesod "App" [parseRoutes|
/plaintext          PlaintextR   GET
/db                 DbR          GET
/queries/#Int       QueriesR     GET
!/queries/#Text      DefaultQueriesR     GET
/fortunes           FortunesR    GET
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

-- getJsonR :: Handler ()
-- getJsonR = sendWaiResponse
--          $ responseBuilder
--             status200
--             [("Content-Type", simpleContentType typeJson)]
--          $ copyByteString
--          $ L.toStrict
--          $ encode
--          $ object ["message" .= ("Hello, World!" :: Text)]

runPg dbAction = do
  app <- getYesod
  runSqlPool dbAction (appDbPool app)

getRandomRow = do
  app <- getYesod
  randomNumber <- liftIO $ ((R.uniformR (1, 1000) (appGen app)) :: IO Int)
  let wId = (toSqlKey $ fromIntegral randomNumber) :: WorldId
  get wId >>= \case
    Nothing -> return Nothing
    Just x -> return $ Just (Entity wId x)

getDbR :: Handler Value
getDbR = do
  (runPg getRandomRow) >>= \case
    -- TODO: Throw appropriate HTTP response
    Nothing -> error "This shouldn't be happening"
    Just worldE -> returnJson worldE

getQueriesR :: Int -> Handler Value
getQueriesR cnt = do
  resultMaybe <- (runPg $ forM [1..sanitizedCnt] (\_ -> getRandomRow))
  let result = map fromJust resultMaybe
  returnJson result
  where
    sanitizedCnt
      | cnt<1 = 1
      | cnt>500 = 500
      | otherwise = cnt

getDefaultQueriesR :: Text -> Handler Value
getDefaultQueriesR txt = getQueriesR 1

getFortunesR :: Handler Html
getFortunesR = do
  fortunesFromDb <- runPg $ selectList [] []
  let fortunes = sortBy (compare `on` fortuneMessage . entityVal) $ (Entity (toSqlKey 0) Fortune{fortuneMessage="Additional fortune added at request time."}):fortunesFromDb
  defaultLayout [whamlet|
                      <table>
                        <tr>
                          <th>id
                          <th>message
                        $forall fortune <- fortunes
                          <tr>
                            <td>#{entityKey fortune}
                            <td>#{fortuneMessage $ entityVal fortune}
                          |]

-- Getmongorawdbr :: Handler Value
-- getMongoRawDbR = getDb rawMongoIntQuery

-- getDbsR :: Int -> Handler Value
-- getDbsR cnt = do
--     App {..} <- getYesod
--     multiRandomHandler randomNumber (intQuery runMySQL My.toSqlKey) cnt'
--   where
--     cnt' | cnt < 1 = 1
--          | cnt > 500 = 500
--          | otherwise = cnt

-- getDbsDefaultR :: Text -> Handler Value
-- getDbsDefaultR _ = getDbsR 1

-- getMongoRawDbsR :: Int -> Handler Value
-- getMongoRawDbsR cnt = multiRandomHandler randomNumber rawMongoIntQuery cnt'
--   where
--     cnt' | cnt < 1 = 1
--          | cnt > 500 = 500
--          | otherwise = cnt

-- getMongoRawDbsDefaultR :: Text -> Handler Value
-- getMongoRawDbsDefaultR _ = getMongoRawDbsR 1

-- getUpdatesR :: Int -> Handler Value
-- getUpdatesR cnt = multiRandomHandler randomPair go cnt'
--   where
--     cnt' | cnt < 1 = 1
--          | cnt > 500 = 500
--          | otherwise = cnt
--     go = uncurry (intUpdate runMySQL My.toSqlKey)

-- getUpdatesDefaultR :: Text -> Handler Value
-- getUpdatesDefaultR _ = getUpdatesR 1

-- randomNumber :: R.Gen (PrimState IO) -> IO Int64
-- randomNumber appGen = R.uniformR (1, 10000) appGen

-- randomPair :: R.Gen (PrimState IO) -> IO (Int64, Int64)
-- randomPair appGen = liftA2 (,) (randomNumber appGen) (randomNumber appGen)

-- getDb :: (Int64 -> Handler Value) -> Handler Value
-- getDb query = do
--     app <- getYesod
--     i <- liftIO (randomNumber (appGen app))
--     value <- query i
--     sendWaiResponse
--         $ responseBuilder
--             status200
--             [("Content-Type", simpleContentType typeJson)]
--         $ copyByteString
--         $ L.toStrict
--         $ encode value


-- runMongoDB :: Mongo.Action Handler b -> Handler b
-- runMongoDB f = do
--   App {..} <- getYesod
--   withResource mongoDBPool $ \pipe ->
--     Mongo.access pipe Mongo.ReadStaleOk "hello_world" f

-- runMySQL :: My.SqlPersistT Handler b -> Handler b
-- runMySQL f = do
--   App {..} <- getYesod
--   My.runSqlPool f mySqlPool

-- intQuery :: (MonadIO m, PersistEntity val, PersistStore backend
--             , backend ~ PersistEntityBackend val
--             ) =>
--            (ReaderT backend m (Maybe val) -> m (Maybe (WorldGeneric backend)))
--            -> (Int64 -> Key val)
--            -> Int64 -> m Value
-- intQuery db toKey i = do
--     Just x <- db $ get $ toKey i
--     return $ jsonResult (worldRandomNumber x)
--   where
--     jsonResult :: Int -> Value
--     jsonResult n = object ["id" .= i, "randomNumber" .= n]

-- rawMongoIntQuery :: Mongo.Val v => v -> Handler Value
-- rawMongoIntQuery i = do
--     Just x <- runMongoDB $ Mongo.findOne (Mongo.select ["id" =: i] "World")
--     return $ documentToJson x

-- intUpdate :: (Functor m, Monad m, MonadIO m
--              , PersistStore backend) =>
--              (ReaderT backend m (Maybe (WorldGeneric backend))
--                 -> m (Maybe (WorldGeneric backend)))
--              -> (Int64 -> Key (WorldGeneric backend))
--              -> Int64 -> Int64 -> m Value
-- intUpdate db toKey i v = do
--     Just x <- db $ get k
--     _ <- db $ fmap (const Nothing) $
--              update k [WorldRandomNumber =. fromIntegral v]
--     return $ object ["id" .= i, "randomNumber" .= v]
--   where
--     k = toKey i

-- multiRandomHandler :: ToJSON a
--                    => (R.Gen (PrimState IO) -> IO b)
--                    -> (b -> Handler a)
--                    -> Int
--                    -> Handler Value
-- multiRandomHandler rand operation cnt = do
--     App {..} <- getYesod
--     nums <- liftIO $ replicateM cnt (rand appGen)
--     return . array =<< mapM operation nums

-- documentToJson :: [Field] -> Value
-- documentToJson = object . map toAssoc
--   where
--     toAssoc :: Field -> (Text, Value)
--     toAssoc ("_id" := v) = ("id", toJSON v)
--     toAssoc (l := v) = (l, toJSON v)

-- instance ToJSON Mongo.Value where
--   toJSON (Mongo.Int32 i)  = toJSON i
--   toJSON (Mongo.Int64 i)  = toJSON i
--   toJSON (Mongo.Float f)  = toJSON f
--   toJSON (Mongo.Doc d)   = documentToJson d
--   toJSON s = error $ "no convert for: " ++ show s

-- getFortunesR :: Handler ()
-- getFortunesR = do
--     es <- runMySQL $ My.selectList [] []
--     sendWaiResponse
--         $ responseBuilder status200 [("Content-type", typeHtml)]
--         $ fortuneTemplate (messages es)
--   where
--     messages es = sortBy (compare `on` snd)
--         ((0, "Additional fortune added at request time.") : map stripEntity es)
--     stripEntity e =
--         (My.fromSqlKey (My.entityKey e), fortuneMessage . My.entityVal $ e)

getPlaintextR :: Handler Text
getPlaintextR = return "Hello, World!"

-- sendWaiResponse
--   $ responseBuilder
--   status200
--   [("Content-Type", simpleContentType typePlain)]
--   $ copyByteString 

-- fortuneTemplate :: [(Int64, Text)] -> Builder
-- fortuneTemplate messages = renderHtmlBuilder $ [shamlet|
-- $doctype 5
-- <html>
--     <head>
--         <title>Fortunes
--     <body>
--         <table>
--             <tr>
--                 <th>id
--                 <th>message
--             $forall message <- messages
--                 <tr>
--                     <td>#{fst message}
--                     <td>#{snd message}
-- |]



main :: IO ()
main = R.withSystemRandom $ \gen -> do
    [cores, host] <- getArgs
    let connString = ("host=" ++ host ++ " port=5432 user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world") 
    dbPool <- runNoLoggingT $ Pg.createPostgresqlPool (C8.pack connString) 256
    app <- toWaiAppPlain App
        { appGen = gen
        , appDbPool = dbPool
        }

    runInUnboundThread $ Warp.runSettings
        ( Warp.setPort 8000
        $ Warp.setHost "*"
        $ Warp.setOnException (\_ _ -> return ())
          Warp.defaultSettings
        ) app

