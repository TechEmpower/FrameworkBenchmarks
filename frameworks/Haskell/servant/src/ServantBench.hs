{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module ServantBench (run) where

import           Control.Exception          (bracket)
import           Control.Monad              (replicateM)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 hiding (json)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy
import           Data.Functor.Contravariant (contramap)
import           Data.Int                   (Int32)
import           Data.List                  (sortOn)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import           GHC.Exts                   (IsList (fromList))
import           GHC.Generics               (Generic)
import qualified Hasql.Decoders             as HasqlDec
import qualified Hasql.Encoders             as HasqlEnc
import           Hasql.Pool                 (Pool, acquire, release, use)
import qualified Hasql.Query                as Hasql
import           Hasql.Session              (query)
import           Lucid
import qualified Network.Wai.Handler.Warp   as Warp
import           Network.HTTP.Media         ((//))
import           Servant
import           Servant.HTML.Lucid         (HTML)
import           System.Random.MWC          (GenIO, createSystemRandom,
                                             uniformR)

type API =
       "json" :> Get '[JSON] Value
  :<|> "db" :> Get '[JSON] World
  :<|> "queries" :> QueryParam "queries" Int :> Get '[JSON] [World]
  :<|> "fortune" :> Get '[HTML] (Html ())
  :<|> "updates" :> QueryParam "queries" Int :> Get '[JSON] [World]
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
    settings = (30, halfSecond, dbSettings)


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
intValEnc = HasqlEnc.value HasqlEnc.int4
intValDec :: HasqlDec.Row Int32
intValDec = HasqlDec.value HasqlDec.int4

-- * PlainText without charset

data Plain
instance Accept Plain where contentType _ = "text" // "plain"
instance MimeRender Plain ByteString where
  mimeRender _ = id
  {-# INLINE mimeRender #-}

------------------------------------------------------------------------------

-- * Test 1: JSON serialization

json :: Handler Value
json = return . Object $ fromList [("message", "Hello, World!")]
{-# INLINE json #-}


-- * Test 2: Single database query

selectSingle :: Hasql.Query Int32 World
selectSingle = Hasql.statement q intValEnc decoder True
  where
   q = "SELECT * FROM World WHERE (id = $1)"
   decoder = HasqlDec.singleRow $ World <$> intValDec <*> intValDec
{-# INLINE selectSingle #-}

singleDb :: Pool -> GenIO -> Handler World
singleDb pool gen = do
  v <- liftIO $ uniformR (1, 10000) gen
  r <- liftIO $ use pool (query v selectSingle)
  case r of
    Left e -> throwError err500
    Right world -> return world
{-# INLINE singleDb #-}


-- * Test 3: Multiple database query

multipleDb :: Pool -> GenIO -> Maybe Int -> Handler [World]
multipleDb pool gen mcount = replicateM count $ singleDb pool gen
  where
    count = let c = fromMaybe 1 mcount in max 1 (min c 500)
{-# INLINE multipleDb #-}


-- * Test 4: Fortunes

selectFortunes :: Hasql.Query () [Fortune]
selectFortunes = Hasql.statement q encoder decoder True
  where
   q = "SELECT * FROM Fortune"
   encoder = HasqlEnc.unit
   -- TODO: investigate whether 'rowsList' is worth the more expensive 'cons'.
   decoder = HasqlDec.rowsList $ Fortune <$> intValDec <*> HasqlDec.value HasqlDec.text
{-# INLINE selectFortunes #-}

fortunes :: Pool -> Handler (Html ())
fortunes pool = do
  r <- liftIO $ use pool (query () selectFortunes)
  case r of
    Left e -> throwError err500
    Right fs -> return $ do
      let new = Fortune 0 "Additional fortune added at request time."
      doctypehtml_ $ do
        head_ $ title_ "Fortunes"
        body_ $ do
          table_ $ do
            tr_ $ do
              th_ "id"
              th_ "message"
            mapM_ (\f -> tr_ $ do
              td_ (toHtml . show $ fId f)
              td_ (toHtml $ fMessage f)) (sortOn fMessage (new : fs))
{-# INLINE fortunes #-}


-- * Test 5: Updates

updateSingle :: Hasql.Query (Int32, Int32) World
updateSingle = Hasql.statement q encoder decoder True
  where
    q = "UPDATE World SET randomNumber = $1 WHERE id = $2"
    encoder = contramap fst intValEnc <> contramap snd intValEnc
    decoder = HasqlDec.singleRow $ World <$> intValDec <*> intValDec
{-# INLINE updateSingle #-}

updates :: Pool -> GenIO -> Maybe Int -> Handler [World]
updates pool gen mcount = replicateM count $ do
  res <- singleDb pool gen
  v <- liftIO $ uniformR (1, 10000) gen
  r <- liftIO $ use pool (query (wId res, v) updateSingle)
  return $ res { wRandomNumber = v }
  where
    count = let c = fromMaybe 1 mcount in max 1 (min c 500)
{-# INLINE updates #-}

-- * Test 6: Plaintext endpoint

plaintext :: Handler ByteString
plaintext = return "Hello, World!"
{-# INLINE plaintext #-}
