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
import           Control.Monad (forM, forM_)

import qualified Data.Pool as Pool
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Database.MySQL.Base as MySQL
import qualified System.IO.Streams as Streams
import           Data.Text (Text)
import qualified Data.Text as Text

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

type Connection = MySQL.MySQLConn
type Pool = Pool.Pool Connection
type Error = Text
type DbRow = [MySQL.MySQLValue]

connect :: Config -> IO Connection
connect c = MySQL.connect myc
  where
    myc = MySQL.defaultConnectInfoMB4
        { MySQL.ciHost     = configHost c
        , MySQL.ciDatabase = configName c
        , MySQL.ciUser     = configUser c
        , MySQL.ciPassword = configPass c
        }

close :: Connection -> IO ()
close = MySQL.close

mkPool :: Config -> IO Pool
mkPool c = Pool.createPool (connect c) close (configStripes c) 0.5 (configPoolSize c)

{-# SPECIALIZE intValEnc :: Int -> MySQL.MySQLValue #-}
{-# SPECIALIZE intValEnc :: Types.QId -> MySQL.MySQLValue #-}
intValEnc :: Integral a => a -> MySQL.MySQLValue
intValEnc = MySQL.MySQLInt16U . fromIntegral

intValDec :: MySQL.MySQLValue -> Either Text Int
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

-------------------------------------------------------------------------------
-- * World

decodeWorld :: DbRow -> Either Error Types.World
decodeWorld [] = Left "MarshalError: Expected 2 columns for World, found 0"
decodeWorld (_:[]) = Left "MarshalError: Expected 2 columns for World, found 1"
decodeWorld (c1:c2:_) = Types.World <$> intValDec c1 <*> intValDec c2

queryWorldById :: Pool -> Types.QId -> IO (Either Error Types.World)
queryWorldById dbPool wId = Pool.withResource dbPool $ \conn -> do
  (_, rowsS) <- MySQL.query conn s [intValEnc wId]
  rows <- Streams.toList rowsS
  let eWorlds = fmap decodeWorld rows
  let (err, oks) = Either.partitionEithers eWorlds
  return $ case err of
    [] -> case oks of
      [] -> Left "World not found!"
      ws  -> pure $ head ws
    _ -> Left . mconcat $ err
  where
    s = "SELECT * FROM World WHERE id = ?"

queryWorldByIds :: Pool -> [Types.QId] -> IO (Either Error [Types.World])
queryWorldByIds _ [] = pure . pure $ mempty
queryWorldByIds dbPool wIds = Pool.withResource dbPool $ \conn -> do
  sId <- MySQL.prepareStmt conn "SELECT * FROM World WHERE id = ?"
  res <- forM wIds $ \wId -> do
    (_, rowsS) <- MySQL.queryStmt conn sId [intValEnc wId]
    rows <- Streams.toList rowsS
    return . fmap decodeWorld $ rows
  MySQL.closeStmt conn sId
  let (errs, ws) = Either.partitionEithers . mconcat $ res
  return $ case errs of
    [] -> pure ws
    _ -> Left . mconcat $ errs

updateWorlds :: Pool -> [(Types.World, Int)] -> IO (Either Error [Types.World])
updateWorlds _ [] = pure . pure $ mempty
updateWorlds dbPool wsUpdates = Pool.withResource dbPool $ \conn -> do
  let ws = fmap updateW wsUpdates
  sId <- MySQL.prepareStmt conn "UPDATE World SET randomNumber = ? WHERE id = ?"
  forM_ wsUpdates $ \(w, wNum) ->
    MySQL.executeStmt conn sId [intValEnc wNum, intValEnc $ Types.wId w]
  MySQL.closeStmt conn sId
  return . pure $ ws
  where
    updateW (w,wNum) = w { Types.wRandomNumber = wNum }

-------------------------------------------------------------------------------
-- * Fortunes

decodeFortune :: DbRow -> Either Error Types.Fortune
decodeFortune [] = Left "MarshalError: Expected 2 columns for Fortune, found 0"
decodeFortune (_:[]) = Left "MarshalError: Expected 2 columns for Fortune, found 1"
decodeFortune (c1:c2:_) = Types.Fortune <$> intValDec c1 <*> textValDec c2

queryFortunes :: Pool -> IO (Either Error [Types.Fortune])
queryFortunes dbPool = Pool.withResource dbPool $ \conn -> do
  (_, rowsS) <- MySQL.query_ conn "SELECT * FROM Fortune"
  rows <- Streams.toList rowsS
  let eFortunes = fmap decodeFortune rows
  let (err, oks) = Either.partitionEithers eFortunes
  return $ case err of
    [] -> pure oks
    _ -> Left $ head err
