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
import           Control.Monad (forM, forM_)

import qualified Hasql.Decoders             as HasqlDec
import qualified Hasql.Encoders             as HasqlEnc
import           Hasql.Pool                 (Pool, acquire, UsageError, use)
import qualified Hasql.Statement            as HasqlStatement
import           Hasql.Session              (statement)
import           Hasql.Connection           (settings, Settings)
import           Data.Functor.Contravariant (contramap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

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

type Error = UsageError

mkSettings :: Config -> Settings
mkSettings c = settings (BSC.pack $ configHost c) 5432 (configUser c) (configPass c) (configName c)

mkPool :: Config -> IO Pool
mkPool c = acquire (configPoolSize c, 0.5, mkSettings c)

intValEnc :: HasqlEnc.Params Types.QId
intValEnc = contramap fromIntegral $ HasqlEnc.param HasqlEnc.int2
intValDec :: HasqlDec.Row Types.QId
intValDec = fmap fromIntegral $ HasqlDec.column HasqlDec.int2

-------------------------------------------------------------------------------
-- * World

selectSingle :: HasqlStatement.Statement Types.QId Types.World
selectSingle = HasqlStatement.Statement q intValEnc decoder True
  where
   q = "SELECT * FROM World WHERE (id = $1)"
   decoder = HasqlDec.singleRow $ Types.World <$> intValDec <*> intValDec

queryWorldById :: Pool -> Types.QId -> IO (Either Error Types.World)
queryWorldById pool wId = use pool (statement wId selectSingle)

queryWorldByIds :: Pool -> [Types.QId] -> IO (Either Error [Types.World])
queryWorldByIds _ [] = pure . pure $ mempty
queryWorldByIds pool wIds = use pool $ do
  forM wIds $ \wId -> statement wId selectSingle

updateSingle :: HasqlStatement.Statement (Types.QId, Types.QId) ()
updateSingle = HasqlStatement.Statement q encoder decoder True
  where
    q = "UPDATE World SET randomNumber = $1 WHERE id = $2"
    encoder = contramap fst intValEnc <> contramap snd intValEnc
    decoder = HasqlDec.unit

updateWorlds :: Pool -> [(Types.World, Types.QId)] -> IO (Either Error [Types.World])
updateWorlds _ [] = pure . pure $ mempty
updateWorlds pool wsUpdates = use pool $ do
  let ws = fmap updateW wsUpdates
  forM_ wsUpdates $ \(w, wNum) -> do
    statement (Types.wId w, wNum) updateSingle
  return ws
  where
    updateW (w,wNum) = w { Types.wRandomNumber = wNum }

-------------------------------------------------------------------------------
-- * Fortunes

selectFortunes :: HasqlStatement.Statement () [Types.Fortune]
selectFortunes = HasqlStatement.Statement q encoder decoder True
  where
   q = "SELECT * FROM Fortune"
   encoder = HasqlEnc.unit
   -- TODO: investigate whether 'rowList' is worth the more expensive 'cons'.
   decoder = HasqlDec.rowList $ Types.Fortune <$> intValDec <*> HasqlDec.column HasqlDec.text
{-# INLINE selectFortunes #-}

queryFortunes :: Pool -> IO (Either Error [Types.Fortune])
queryFortunes pool = use pool (statement () selectFortunes)
