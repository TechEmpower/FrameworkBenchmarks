{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings     #-}

module TFB.Db (
    Pool
  , mkPool
  , Config(..)
  , queryWorldById
  , queryWorldByIds
  , updateWorlds
  , queryFortunes
  , Error
) where

import qualified TFB.Types as Types
import qualified Data.Either as Either
import qualified System.IO.Error as Error
import           Control.Monad (replicateM, forM)

import qualified Data.Pool as Pool
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Database.PostgreSQL.Driver as PG
import qualified Database.PostgreSQL.Protocol.Types as PGT
import qualified Database.PostgreSQL.Protocol.DataRows as PGD
import qualified Database.PostgreSQL.Protocol.Store.Decode as PGSD
import qualified Database.PostgreSQL.Protocol.Store.Encode as PGSE
import qualified Database.PostgreSQL.Protocol.Codecs.Decoders as PGCD
import qualified Database.PostgreSQL.Protocol.Codecs.Encoders as PGCE
import qualified Database.PostgreSQL.Protocol.Codecs.PgTypes as PGCT
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE

-------------------------------------------------------------------------------
-- * Database

data Config
  = Config
  { configHost      :: String
  , configName      :: ByteString
  , configUser      :: ByteString
  , configPass      :: ByteString
  , configStripes   :: Int
  , configPoolSize  :: Int
  }
instance Show Config where
  show c
    =  "Config {"
    <>  " configHost = " <> configHost c
    <> ", configName = " <> BSC.unpack (configName c)
    <> ", configUser = " <> BSC.unpack (configUser c)
    <> ", configPass = REDACTED"
    <> ", configStripes = " <> show (configStripes c)
    <> ", configPoolSize = " <> show (configPoolSize c)
    <> " }"

type Connection = PG.Connection
type Pool = Pool.Pool Connection
data Error
  = DbError PG.Error
  | DbErrors [PG.Error]
  | NotFound
  deriving Show

connect :: Config -> IO Connection
connect c = simplifyError =<< PG.connect pgc
  where
    simplifyError = Either.either (Error.ioError . Error.userError . show) pure
    pgc = PG.defaultConnectionSettings
        { PG.settingsHost     = BSC.pack $ configHost c
        , PG.settingsDatabase = configName c
        , PG.settingsUser     = configUser c
        , PG.settingsPassword = configPass c
        }

close :: Connection -> IO ()
close = PG.close

mkPool :: Config -> IO Pool
mkPool c = Pool.createPool (connect c) close (configStripes c) 0.5 (configPoolSize c)

runQuery :: Connection -> PGSD.Decode a -> PG.Query -> IO (Either PG.Error (V.Vector a))
runQuery conn dec q = do
  PG.sendBatchAndSync conn [q]
  eRows <- PG.readNextData conn
  _ <- PG.waitReadyForQuery conn
  return $ fmap (PGD.decodeManyRows dec) eRows

decodeInt :: PGSD.Decode Int
decodeInt = fromIntegral <$> PGCD.getNonNullable PGCD.int4

decodeText :: PGSD.Decode Text
decodeText = TE.decodeUtf8 <$> PGCD.getNonNullable PGCD.bytea

encodeInt :: Integral a => a -> (PGCT.Oids, PGSE.Encode)
encodeInt qId = (PGCT.int2, PGCE.int2 $ fromIntegral qId)

mkQuery :: ByteString -> [(PGCT.Oids, PGSE.Encode)] -> PG.Query
mkQuery q es = PG.Query q ps PGT.Binary PGT.Binary PG.NeverCache
  where
    mkP (oid, e) = (PGCT.oidType oid, Just e)
    ps = fmap mkP es

-------------------------------------------------------------------------------
-- * World

decodeWorld :: PGSD.Decode Types.World
decodeWorld = PGCD.dataRowHeader *> decoder
  where 
    decoder = Types.World
        <$> decodeInt
        <*> decodeInt

queryWorldById :: Pool -> Types.QId -> IO (Either Error Types.World)
queryWorldById dbPool wId = Pool.withResource dbPool $ \conn -> do
  fmap go $ runQuery conn decodeWorld q
  where
    s = "SELECT * FROM World WHERE id = $1"
    q = mkQuery s [encodeInt wId]
    mkW [] = Left NotFound
    mkW ws = pure . head $ ws
    go = Either.either (Left . DbError) (mkW . V.toList)

queryWorldByIds :: Pool -> [Types.QId] -> IO (Either Error [Types.World])
queryWorldByIds _ [] = pure . pure $ mempty
queryWorldByIds dbPool wIds = Pool.withResource dbPool $ \conn -> do
  let s = "SELECT * FROM World WHERE id = $1"
  let mkQ wId = mkQuery s [encodeInt wId]
  let qs = fmap mkQ wIds
  PG.sendBatchAndSync conn qs
  eRowsMany <- replicateM (length qs) $ PG.readNextData conn
  _ <- PG.waitReadyForQuery conn
  let (errs, rowsList) = Either.partitionEithers eRowsMany
  return $ case errs of
    [] -> pure . mconcat $ fmap (V.toList . PGD.decodeManyRows decodeWorld) rowsList
    _ -> Left . DbErrors $ errs

updateWorlds :: Pool -> [(Types.World, Int)] -> IO (Either Error [Types.World])
updateWorlds _ [] = pure . pure $ mempty
updateWorlds dbPool wsUpdates = Pool.withResource dbPool $ \conn -> do
  let ws = fmap updateW wsUpdates
  let qs = fmap mkQ ws
  eRowsMany <- forM qs $ \q -> do
    PG.sendBatchAndSync conn [q]
    eRows <- PG.readNextData conn
    _ <- PG.waitReadyForQuery conn
    return eRows
  let (errs, _) = Either.partitionEithers eRowsMany
  return $ case errs of
    [] -> pure ws
    _ -> Left . DbErrors $ errs
  where
    s = "UPDATE World SET randomNumber = $1 WHERE id = $2"
    updateW (w,wNum) = w { Types.wRandomNumber = wNum }
    mkQ w = mkQuery s [encodeInt . Types.wRandomNumber $ w, encodeInt . Types.wId $ w]

-------------------------------------------------------------------------------
-- * Fortunes

decodeFortune :: PGSD.Decode Types.Fortune
decodeFortune = PGCD.dataRowHeader *> decoder
  where 
    decoder = Types.Fortune
        <$> decodeInt
        <*> decodeText

queryFortunes :: Pool -> IO (Either Error [Types.Fortune])
queryFortunes dbPool = Pool.withResource dbPool $ \conn -> do
  fmap go $ runQuery conn decodeFortune q
  where
    s = "SELECT * FROM Fortune"
    q = mkQuery s []
    go = Either.either (Left . DbError) (pure . V.toList)
