{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module ServantBench (run) where

import           Control.Exception          (bracket)
import           Control.Monad              (replicateM)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 hiding (json)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Functor.Contravariant (contramap)
import           Data.Either                (fromRight, partitionEithers)
import           Data.Int                   (Int32)
import           Data.List                  (sortOn)
import           Data.Maybe                 (maybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import           Data.Text                  (Text)
import           GHC.Exts                   (IsList (fromList))
import           GHC.Generics               (Generic)
import qualified Hasql.Decoders             as HasqlDec
import qualified Hasql.Encoders             as HasqlEnc
import           Hasql.Pool                 (Pool, acquire, release, use)
import qualified Hasql.Statement            as HasqlStatement
import           Hasql.Session              (statement)
import qualified Html
import           Html ((#), type (#), type (>))
import qualified Network.Wai.Handler.Warp   as Warp
import           Network.HTTP.Media         ((//), (/:))
import           Servant
import           System.Random.MWC          (GenIO, createSystemRandom,
                                             uniformR)
import qualified Data.List.NonEmpty as NE

type API =
       "json" :> Get '[JSON] Value
  :<|> "db" :> Get '[JSON] World
  :<|> "queries" :> QueryParam "queries" QueryId :> Get '[JSON] [World]
  :<|> "fortune" :> Get '[HTML] FortunesHtml
  :<|> "updates" :> QueryParam "queries" QueryId :> Get '[JSON] [World]
  :<|> "plaintext" :> Get '[Plain] ByteString

api :: Proxy API
api = Proxy

server :: Pool -> GenIO -> Server API
server pool gen =
      json
 :<|> singleDb pool gen
 :<|> multipleDb pool gen
 :<|> fortunes pool
 :<|> updates pool gen
 :<|> plaintext

run :: Warp.Port -> BS.ByteString -> IO ()
run port dbSettings = do
  gen <- createSystemRandom
  bracket (acquire settings) release $ \pool ->
    Warp.run port $ serve api $ server pool gen
  where
    halfSecond = 0.5
    settings = (512, halfSecond, dbSettings)

newtype QueryId = QueryId { unQueryId :: Int }
instance FromHttpApiData QueryId where
  parseQueryParam
    = pure . QueryId . fromRight 1 . parseQueryParam

data World = World { wId :: !Int32 , wRandomNumber :: !Int32 }
  deriving (Show, Generic)

instance ToJSON World where
  toEncoding w =
    pairs (  "id"           .= wId w
          <> "randomNumber" .= wRandomNumber w
          )

data Fortune = Fortune { fId :: !Int32 , fMessage :: Text.Text }
  deriving (Show, Generic)

instance ToJSON Fortune where
  toEncoding f =
    pairs (  "id"      .= fId f
          <> "message" .= fMessage f
          )

intValEnc :: HasqlEnc.Params Int32
intValEnc = HasqlEnc.param HasqlEnc.int4
intValDec :: HasqlDec.Row Int32
intValDec = HasqlDec.column HasqlDec.int4

-- * PlainText without charset

data Plain
instance Accept Plain where contentType _ = "text" // "plain"
instance MimeRender Plain ByteString where
  mimeRender _ = id
  {-# INLINE mimeRender #-}
  
-- * HTML
-- TODO: package the following block of code into a library akin to 'servant-lucid'

data HTML
instance Accept HTML where
    contentTypes _ =
      "text" // "html" /: ("charset", "utf-8") NE.:|
      ["text" // "html"]
instance Html.Document a => MimeRender HTML a where
    mimeRender _ = Html.renderByteString

------------------------------------------------------------------------------

-- * Test 1: JSON serialization

json :: Handler Value
json = return . Object $ fromList [("message", "Hello, World!")]
{-# INLINE json #-}


-- * Test 2: Single database query

selectSingle :: HasqlStatement.Statement Int32 World
selectSingle = HasqlStatement.Statement q intValEnc decoder True
  where
   q = "SELECT * FROM World WHERE (id = $1)"
   decoder = HasqlDec.singleRow $ World <$> intValDec <*> intValDec
{-# INLINE selectSingle #-}

singleDb :: Pool -> GenIO -> Handler World
singleDb pool gen = do
  v <- liftIO $ uniformR (1, 10000) gen
  r <- liftIO $ use pool (statement v selectSingle)
  case r of
    Left e -> throwError err500 { errBody = LBSC.pack . show $ e }
    Right world -> return world
{-# INLINE singleDb #-}


-- * Test 3: Multiple database query

multipleDb :: Pool -> GenIO -> Maybe QueryId -> Handler [World]
multipleDb pool gen mQueryId = do
  results <- getResults
  let (errs, oks) = partitionEithers results
  case errs of
    [] -> return oks
    _ -> throwError err500 { errBody = LBSC.pack . show $ errs }
  where
    c = maybe 1 unQueryId mQueryId
    count_ = max 1 (min c 500)
    getResults = replicateM count_ . liftIO . use pool $ do
      v <- liftIO $ uniformR (1, 10000) gen
      statement v selectSingle
{-# INLINE multipleDb #-}


-- * Test 4: Fortunes

type FortunesHtml
  = (('Html.DOCTYPE Html.> ())
  # ('Html.Html
    > (('Html.Head > ('Html.Title > Html.Raw Text))
      # ('Html.Body
        > ('Html.Table
          > (
              ('Html.Tr
              > ( ('Html.Th > Html.Raw Text)
                # ('Html.Th > Html.Raw Text)
                )
              )
            # ['Html.Tr
              > ( ('Html.Td > Int)
                # ('Html.Td > Text)
                )
              ]
            )
          )
        )
      )
    )
  )

selectFortunes :: HasqlStatement.Statement () [Fortune]
selectFortunes = HasqlStatement.Statement q encoder decoder True
  where
   q = "SELECT * FROM Fortune"
   encoder = HasqlEnc.unit
   -- TODO: investigate whether 'rowList' is worth the more expensive 'cons'.
   decoder = HasqlDec.rowList $ Fortune <$> intValDec <*> HasqlDec.column HasqlDec.text
{-# INLINE selectFortunes #-}

fortunes :: Pool -> Handler FortunesHtml
fortunes pool = do
  r <- liftIO $ use pool (statement () selectFortunes)
  case r of
    Left e -> throwError err500 { errBody = LBSC.pack . show $ e }
    Right fs -> return $ do
      let new = Fortune 0 "Additional fortune added at request time."
      let header = Html.tr_ $ Html.th_ (Html.Raw "id") # Html.th_ (Html.Raw "message")
      let mkRow f = Html.tr_ $ Html.td_ (fromIntegral $ fId f) # Html.td_ (fMessage f)
      let rows = fmap mkRow $ sortOn fMessage (new : fs)
      Html.doctype_ #
        Html.html_ (
          Html.head_ (
            Html.title_ (Html.Raw "Fortunes")
          ) #
          Html.body_ ( Html.table_ $
            header # rows
          )
        )
{-# INLINE fortunes #-}


-- * Test 5: Updates

updateSingle :: HasqlStatement.Statement (Int32, Int32) ()
updateSingle = HasqlStatement.Statement q encoder decoder True
  where
    q = "UPDATE World SET randomNumber = $1 WHERE id = $2"
    encoder = contramap fst intValEnc <> contramap snd intValEnc
    decoder = HasqlDec.unit
{-# INLINE updateSingle #-}

updates :: Pool -> GenIO -> Maybe QueryId -> Handler [World]
updates pool gen mQueryId = do
  results <- getResults
  let (errs, oks) = partitionEithers results
  case errs of
    [] -> return oks
    _ -> throwError err500 { errBody = LBSC.pack . show $ errs }
  where
    c = maybe 1 unQueryId mQueryId
    count_ = max 1 (min c 500)
    getResults = replicateM count_ . liftIO . use pool $ do
      v1 <- liftIO $ uniformR (1, 10000) gen
      res <- statement v1 selectSingle
      v2 <- liftIO $ uniformR (1, 10000) gen
      _ <- statement (wId res, v2) updateSingle
      return $ res { wRandomNumber = v2 }
{-# INLINE updates #-}

-- * Test 6: Plaintext endpoint

plaintext :: Handler ByteString
plaintext = return "Hello, World!"
{-# INLINE plaintext #-}
