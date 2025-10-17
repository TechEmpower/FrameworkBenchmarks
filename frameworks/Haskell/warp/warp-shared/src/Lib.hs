{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main,
    Db.Config (..),
  )
where

import Control.Monad (join, replicateM)
import Data.BufferBuilder.Json ((.=))
import Data.BufferBuilder.Json qualified as Json
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.Either qualified as Either
import Data.List (sortOn)
import Network.HTTP.Types.Header qualified as Header
import Network.HTTP.Types.Status qualified as Status
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Random.MWC qualified as MWC
import TFB.Db qualified as Db
import TFB.Types qualified as Types
import Text.Blaze.Html.Renderer.Utf8 qualified as Html
import Text.Blaze.Html5 qualified as Html

-- entry point
main :: Db.Config -> IO ()
main dbConfig = do
  putStrLn "Config is:"
  print dbConfig
  putStrLn "Initializing database connection pool..."
  dbPool <- Db.mkPool dbConfig
  putStrLn "Initializing PRNG seed..."
  gen <- MWC.create
  putStrLn "Warp core online"
  Warp.run 7041 $ app gen dbPool

-- router
app :: MWC.GenIO -> Db.Pool -> Wai.Application
app gen dbPool req respond = do
  let qParams = Wai.queryString req
  let mCount = Types.parseCount =<< join (lookup "queries" qParams)
  case (Wai.requestMethod req, Wai.pathInfo req) of
    ("GET", ["plaintext"]) ->
      respond getPlaintext
    ("GET", ["json"]) ->
      respond getJson
    ("GET", ["db"]) ->
      respond =<< getWorld gen dbPool
    ("GET", ["fortunes"]) ->
      respond =<< getFortunes dbPool
    ("GET", ["queries"]) ->
      respond =<< getWorlds gen dbPool mCount
    ("GET", ["updates"]) ->
      respond =<< updateWorlds gen dbPool mCount
    _ -> respond routeNotFound

-- * response helpers

contentText :: Header.ResponseHeaders
contentText = [(Header.hContentType, "text/plain")]

respondText :: Status.Status -> LBS.ByteString -> Wai.Response
respondText code = Wai.responseLBS code contentText

contentJson :: Header.ResponseHeaders
contentJson = [(Header.hContentType, "application/json")]

{-# SPECIALIZE respondJson :: Json.ObjectBuilder -> Wai.Response #-}
{-# SPECIALIZE respondJson :: Types.World -> Wai.Response #-}
respondJson :: (Json.ToJson a) => a -> Wai.Response
respondJson = Wai.responseLBS Status.status200 contentJson . mkBs
  where
    mkBs = LBS.fromStrict . Json.encodeJson

contentHtml :: Header.ResponseHeaders
contentHtml = [(Header.hContentType, "text/html; charset=UTF-8")]

respondHtml :: Html.Html -> Wai.Response
respondHtml = Wai.responseBuilder Status.status200 contentHtml . Html.renderHtmlBuilder

-- * error responses

routeNotFound :: Wai.Response
routeNotFound = respondText Status.status400 "Bad route"

respondInternalError :: LBS.ByteString -> Wai.Response
respondInternalError = respondText Status.status500

respondDbError :: Db.Error -> Wai.Response
respondDbError = respondInternalError . LBSC.pack . show

-- * route implementations

getPlaintext :: Wai.Response
getPlaintext = respondText Status.status200 "Hello, World!"
{-# INLINE getPlaintext #-}

getJson :: Wai.Response
getJson = respondJson $ "message" .= Types.unsafeJsonString "Hello, World!"
{-# INLINE getJson #-}

getWorld :: MWC.GenIO -> Db.Pool -> IO Wai.Response
getWorld gen dbPool = do
  wId <- randomId gen
  res <- Db.queryWorldById dbPool wId
  pure . mkResponse $ res
  where
    mkResponse = Either.either respondDbError respondJson
{-# INLINE getWorld #-}

getWorlds :: MWC.GenIO -> Db.Pool -> Maybe Types.Count -> IO Wai.Response
getWorlds gen dbPool mCount = do
  wIds <- replicateM count $ randomId gen
  res <- Db.queryWorldByIds dbPool wIds
  pure . mkResponse $ res
  where
    count = Types.getCount mCount
    mkResponse = Either.either respondDbError respondJson
{-# INLINE getWorlds #-}

updateWorlds :: MWC.GenIO -> Db.Pool -> Maybe Types.Count -> IO Wai.Response
updateWorlds gen dbPool mCount = do
  wIds <- replicateM count $ randomId gen
  res <- Db.queryWorldByIds dbPool wIds
  Either.either (pure . respondDbError) (go dbPool) res
  where
    count = Types.getCount mCount
    mkResponse = Either.either respondDbError respondJson
    go conn ws = do
      wNumbers <- replicateM count $ randomId gen
      wsUp <- Db.updateWorlds conn . zip ws $ fmap fromIntegral wNumbers
      return $ mkResponse wsUp
{-# INLINE updateWorlds #-}

getFortunes :: Db.Pool -> IO Wai.Response
getFortunes dbPool = do
  res <- Db.queryFortunes dbPool
  return $ case res of
    Left e -> respondDbError e
    Right fs ->
      let new = Types.Fortune 0 "Additional fortune added at request time."
          header = Html.tr $ do
            Html.th $ Html.preEscapedToHtml ("id" :: String)
            Html.th $ Html.preEscapedToHtml ("message" :: String)
          mkRow f = Html.tr $ do
            Html.td $ Html.toHtml ((fromIntegral $ Types.fId f) :: Int)
            Html.td $ Html.toHtml (Types.fMessage f)
          rows = (mkRow <$> sortOn Types.fMessage (new : fs))
       in respondHtml $ Html.docTypeHtml $ do
            Html.head $ do
              Html.title $ Html.preEscapedToHtml ("Fortunes" :: String)
            Html.body $ do
              Html.table $ do
                header
                sequence_ rows
{-# INLINE getFortunes #-}

randomId :: MWC.GenIO -> IO Types.QId
randomId = MWC.uniformR (1, 10000)
