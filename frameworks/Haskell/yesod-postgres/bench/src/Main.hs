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
import           Control.Concurrent            (runInUnboundThread)
import           Control.Monad                 (forM)
import           Control.Monad.Logger          (runNoLoggingT)
import           Control.Monad.Primitive       (PrimState)
import           Control.Monad.Trans.Resource  (InternalState)
import qualified Data.ByteString.Char8 as C8
import           Data.Pool                     (Pool)
import           Data.IORef                    (newIORef)
import           Data.Function                 (on)
import           Data.List                     (sortBy)
import           Data.Text                     (Text)
import           Database.Persist             
import qualified Database.Persist.Postgresql    as Pg
import Database.Persist.Sql
import           Database.Persist.TH           (mkPersist, mpsGeneric,
                                                persistLowerCase, sqlSettings)
import qualified Network.Wai.Handler.Warp      as Warp
import           System.Environment            (getArgs)
import           System.IO.Unsafe              (unsafePerformIO)
import qualified System.Random.MWC             as R
import Text.Blaze.Html
import           Yesod
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


mkYesod "App" [parseRoutes|
/json               JsonR     GET
/plaintext          PlaintextR   GET
/db                 DbR          GET

/queries/#Int       QueriesR     GET
!/queries/#Text      DefaultQueriesR     GET

/fortunes           FortunesR    GET

/updates/#Int       UpdatesR     GET
!/updates/#Text     DefaultUpdatesR GET
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

getJsonR :: Handler Value
getJsonR = returnJson $ object ["message" .= ("Hello, World!" :: Text)]

runPg dbAction = do
  app <- getYesod
  runSqlPool dbAction (appDbPool app)

getRandomRow = do
  app <- getYesod
  randomNumber <- liftIO $ ((R.uniformR (1, 10000) (appGen app)) :: IO Int)
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
  result <- (runPg $ forM [1..sanitizedCnt] (\_ -> fmap fromJust getRandomRow))
  returnJson result
  where
    sanitizedCnt
      | cnt<1 = 1
      | cnt>500 = 500
      | otherwise = cnt

getDefaultQueriesR :: Text -> Handler Value
getDefaultQueriesR _ = getQueriesR 1

getFortunesR :: Handler Html
getFortunesR = do
  fortunesFromDb <- runPg $ selectList [] []
  let fortunes = sortBy (compare `on` fortuneMessage . entityVal) $ (Entity (toSqlKey 0) Fortune{fortuneMessage="Additional fortune added at request time."}):fortunesFromDb
  defaultLayout $ do
      setTitle "Fortunes"
      [whamlet|
              <table>
                <tr>
                  <th>id
                  <th>message
                $forall fortune <- fortunes
                  <tr>
                    <td>#{entityKey fortune}
                    <td>#{fortuneMessage $ entityVal fortune}
                    |]

getUpdatesR :: Int -> Handler Value
getUpdatesR cnt = do
  worldRows <- runPg $ forM [1..sanitizedCount] (\_ -> fmap fromJust getRandomRow)
  app <- getYesod
  updatedWorldRows <- runPg $ mapM (replaceWorldRow app) worldRows
  returnJson updatedWorldRows
  where
    sanitizedCount
      | cnt<1 = 1
      | cnt>500 = 500
      | otherwise = cnt

    replaceWorldRow app (Entity wId wRow) = do
      randomNumber <- liftIO $ ((R.uniformR (1, 10000) (appGen app)) :: IO Int)
      -- TODO: Should I be using replace, or update, or updateGet -- which is
      -- idiomatic Yesod code for this operation?
      let newRow = wRow{worldRandomNumber=randomNumber}
      replace wId newRow
      return (Entity wId newRow)


getDefaultUpdatesR :: Text -> Handler Value
getDefaultUpdatesR _ = getUpdatesR 1

getPlaintextR :: Handler Text
getPlaintextR = return "Hello, World!"

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

