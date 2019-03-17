{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main (main) where

import           Control.Exception          (bracket)
import           Control.Monad              (replicateM)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as LBS
import           Data.Int                   (Int32)
import           Data.List                  (sortOn)
import           Data.Either                (fromRight, partitionEithers)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as TextEnc
import           GHC.Exts                   (IsList (fromList))
import           GHC.Generics               (Generic)
import qualified Data.Pool                  as Pool
import qualified Database.MySQL.Base        as MySQL
import qualified System.IO.Streams          as Streams
import qualified Html
import           Html ((#), type (#), type (>))
import qualified Network.Wai.Handler.Warp   as Warp
import           Network.HTTP.Media         ((//), (/:))
import           Servant
import           System.Random.MWC          (GenIO, createSystemRandom,
                                             uniformR)
import qualified GHC.Conc
import           System.Environment (getArgs)
import qualified Data.List.NonEmpty as NE

type API =
       "json" :> Get '[JSON] Aeson.Value
  :<|> "db" :> Get '[JSON] World
  :<|> "queries" :> QueryParam "queries" Count :> Get '[JSON] [World]
  :<|> "fortune" :> Get '[HTML] FortunesHtml
  :<|> "updates" :> QueryParam "queries" Count :> Get '[JSON] [World]
  :<|> "plaintext" :> Get '[Plain] LBS.ByteString

api :: Proxy API
api = Proxy

server :: DbPool -> GenIO -> Server API
server pool gen =
      json
 :<|> singleDb pool gen
 :<|> multipleDb pool gen
 :<|> fortunes pool
 :<|> updates pool gen
 :<|> plaintext

run :: Warp.Port -> MySQL.ConnectInfo -> IO ()
run port dbSettings = do
  gen <- createSystemRandom
  numCaps <- GHC.Conc.getNumCapabilities
  let mkPool = Pool.createPool (MySQL.connect dbSettings) MySQL.close numCaps 10 512
  bracket mkPool Pool.destroyAllResources $ \pool ->
    Warp.run port $ serve api $ server pool gen

main :: IO ()
main = do
  [host] <- getArgs
  run 7041 $ MySQL.defaultConnectInfoMB4 {
    MySQL.ciHost = host,
    MySQL.ciDatabase = "hello_world",
    MySQL.ciUser = "benchmarkdbuser",
    MySQL.ciPassword = "benchmarkdbpass"
  }

type DbPool = Pool.Pool MySQL.MySQLConn
type DbRow = [MySQL.MySQLValue]

newtype Count = Count Int
instance FromHttpApiData Count where
  parseQueryParam
    = pure . Count . fromRight 1 . parseQueryParam

getCount :: Maybe Count -> Int
getCount Nothing = 1
getCount (Just (Count c)) = max 1 (min c 500)

data World = World { wId :: !Int32 , wRandomNumber :: !Int32 }
  deriving (Show, Generic)

instance Aeson.ToJSON World where
  toEncoding w
    = Aeson.pairs
    (  "id"           .= wId w
    <> "randomNumber" .= wRandomNumber w
    )

data Fortune = Fortune { fId :: !Int32 , fMessage :: Text }
  deriving (Show, Generic)

instance Aeson.ToJSON Fortune where
  toEncoding f
    = Aeson.pairs
    (  "id"      .= fId f
    <> "message" .= fMessage f
    )

intValEnc :: Int32 -> MySQL.MySQLValue
intValEnc = MySQL.MySQLInt32 . fromIntegral

intValDec :: MySQL.MySQLValue -> Either Text Int32
intValDec (MySQL.MySQLInt8U i) = pure . fromIntegral $ i
intValDec (MySQL.MySQLInt8 i) = pure . fromIntegral $ i
intValDec (MySQL.MySQLInt16U i) = pure . fromIntegral $ i
intValDec (MySQL.MySQLInt16 i) = pure . fromIntegral $ i
intValDec (MySQL.MySQLInt32U i) = pure . fromIntegral $ i
intValDec (MySQL.MySQLInt32 i) = pure . fromIntegral $ i
intValDec (MySQL.MySQLInt64U i) = pure . fromIntegral $ i
intValDec (MySQL.MySQLInt64 i) = pure . fromIntegral $ i
intValDec x = Left $ "Expected MySQLInt*, received" <> (Text.pack $ show x)

textValDec :: MySQL.MySQLValue -> Either Text Text
textValDec (MySQL.MySQLText t) = pure t
textValDec x = Left $ "Expected Text, received" <> (Text.pack $ show x)

-- * PlainText without charset

data Plain
instance Accept Plain where contentType _ = "text" // "plain"
instance MimeRender Plain LBS.ByteString where
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

json :: Handler Aeson.Value
json = return . Aeson.Object $ fromList [("message", "Hello, World!")]
{-# INLINE json #-}


-- * Test 2: Single database query

decodeWorld :: DbRow -> Either Text World
decodeWorld [] = Left "MarshalError: Expected 2 columns for World, found 0"
decodeWorld (_:[]) = Left "MarshalError: Expected 2 columns for World, found 1"
decodeWorld (c1:c2:_) = World <$> intValDec c1 <*> intValDec c2
{-# INLINE decodeWorld #-}

extractWorld :: Streams.InputStream DbRow -> IO (Either Text World)
extractWorld rowsS = do
  rows <- Streams.toList rowsS
  return $ case rows of
    [] -> Left "No rows found!"
    (row:_) -> decodeWorld row

singleDb :: DbPool -> GenIO -> Handler World
singleDb pool gen = do
  v <- liftIO $ uniformR (1, 10000) gen
  r <- liftIO $ Pool.withResource pool $ \conn -> do
    (_, rowsS) <- MySQL.query conn "SELECT * FROM World WHERE id = ?" [intValEnc v]
    extractWorld rowsS
  case r of
    Left e -> throwError err500 { errBody = LBS.fromStrict $ TextEnc.encodeUtf8 e }
    Right world -> return world
{-# INLINE singleDb #-}

-- * Test 3: Multiple database query

multipleDb :: DbPool -> GenIO -> Maybe Count -> Handler [World]
multipleDb pool gen mcount = do
  res <- liftIO $ Pool.withResource pool $ \conn -> do
    sId <- MySQL.prepareStmt conn "SELECT * FROM World WHERE id = ?"
    res <- replicateM (getCount mcount) $ do
      v <- uniformR (1, 10000) gen
      (_, rowsS) <- MySQL.queryStmt conn sId [intValEnc v]
      extractWorld rowsS
    MySQL.closeStmt conn sId
    return res
  let (errs, oks) = partitionEithers res
  case errs of
    [] -> return oks
    e -> throwError err500 { errBody = LBS.fromStrict . TextEnc.encodeUtf8 . Text.pack . show $ e }
{-# INLINE multipleDb #-}


-- * Test 4: Fortunes

decodeFortune :: DbRow -> Either Text Fortune
decodeFortune [] = Left "MarshalError: Expected 2 columns for Fortune, found 0"
decodeFortune (_:[]) = Left "MarshalError: Expected 2 columns for Fortune, found 1"
decodeFortune (c1:c2:_) = Fortune <$> intValDec c1 <*> textValDec c2
{-# INLINE decodeFortune #-}

selectFortunes :: MySQL.MySQLConn -> IO (Either [Text] [Fortune])
selectFortunes conn = do
  (_, rowsS) <- MySQL.query_ conn "SELECT * FROM Fortune"
  rows <- Streams.toList rowsS
  let eFortunes = fmap decodeFortune rows
  let (err, oks) = partitionEithers eFortunes
  return $ case err of
    [] -> pure oks
    _ -> Left err
{-# INLINE selectFortunes #-}

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

fortunes :: DbPool -> Handler FortunesHtml
fortunes pool = do
  r <- liftIO $ Pool.withResource pool selectFortunes
  case r of
    Left e -> throwError err500 { errBody = LBS.fromStrict . TextEnc.encodeUtf8 . Text.pack . show $ e }
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

updates :: DbPool -> GenIO -> Maybe Count -> Handler [World]
updates pool gen mcount = do
  res <- liftIO $ Pool.withResource pool $ \conn -> do
    sIdGet <- MySQL.prepareStmt conn "SELECT * FROM World WHERE id = ?"
    sIdPut <- MySQL.prepareStmt conn "UPDATE World SET randomNumber = ? WHERE id = ?"
    res <- replicateM (getCount mcount) $ do
      vGet <- uniformR (1, 10000) gen
      vPut <- uniformR (1, 10000) gen
      (_, rowsS) <- MySQL.queryStmt conn sIdGet [intValEnc vGet]
      eWorld <- extractWorld rowsS
      case eWorld of
        Left e -> return $ Left e
        Right world -> do
          _ <- MySQL.executeStmt conn sIdPut [intValEnc vPut, intValEnc vGet]
          return . pure $ world { wRandomNumber = vPut }
    MySQL.closeStmt conn sIdGet
    MySQL.closeStmt conn sIdPut
    return res
  let (errs, oks) = partitionEithers res
  case errs of
    [] -> return oks
    e -> throwError err500 { errBody = LBS.fromStrict . TextEnc.encodeUtf8 . Text.pack . show $ e }
{-# INLINE updates #-}

-- * Test 6: Plaintext endpoint

plaintext :: Handler LBS.ByteString
plaintext = return "Hello, World!"
{-# INLINE plaintext #-}
