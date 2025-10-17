{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}

module TFB.Db
  ( Pool,
    mkPool,
    Config (..),
    queryWorldById,
    queryWorldByIds,
    updateWorlds,
    queryFortunes,
    Error,
  )
where

import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Functor.Contravariant (contramap)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Connection.Setting as ConnectionSetting
import Hasql.Connection.Setting.Connection (params)
import qualified Hasql.Connection.Setting.Connection.Param as ConnectionParam
import qualified Hasql.Decoders as HasqlDec
import qualified Hasql.Encoders as HasqlEnc
import Hasql.Pool (Pool, UsageError, acquire, use)
import qualified Hasql.Pool.Config as PoolCfg
import Hasql.Session (statement)
import qualified Hasql.Statement as HasqlStatement
import qualified TFB.Types as Types

-------------------------------------------------------------------------------

-- * Database

data Config
  = Config
  { configHost :: String,
    configName :: ByteString,
    configUser :: ByteString,
    configPass :: ByteString,
    configStripes :: Int,
    configPoolSize :: Int
  }

instance Show Config where
  show c =
    "Config {"
      <> " configHost = "
      <> configHost c
      <> ", configName = "
      <> BSC.unpack (configName c)
      <> ", configUser = "
      <> BSC.unpack (configUser c)
      <> ", configPass = REDACTED"
      <> ", configStripes = "
      <> show (configStripes c)
      <> ", configPoolSize = "
      <> show (configPoolSize c)
      <> " }"

type Error = UsageError

mkSettings :: Config -> ConnectionSetting.Setting
mkSettings c =
  ConnectionSetting.connection $
    params
      [ ConnectionParam.host (T.pack $ configHost c),
        ConnectionParam.port 5432,
        ConnectionParam.user (TE.decodeUtf8 $ configUser c),
        ConnectionParam.password (TE.decodeUtf8 $ configPass c),
        ConnectionParam.dbname (TE.decodeUtf8 $ configName c)
      ]

mkPool :: Config -> IO Pool
mkPool c =
  acquire $
    PoolCfg.settings
      [ PoolCfg.staticConnectionSettings [mkSettings c],
        PoolCfg.size (configPoolSize c)
      ]

qidEnc :: HasqlEnc.Params Types.QId
qidEnc = contramap fromIntegral (HasqlEnc.param (HasqlEnc.nonNullable HasqlEnc.int4))

qidDec :: HasqlDec.Row Types.QId
qidDec = fromIntegral <$> (HasqlDec.column . HasqlDec.nonNullable) HasqlDec.int4

-------------------------------------------------------------------------------

-- * World

selectSingle :: HasqlStatement.Statement Types.QId Types.World
selectSingle = HasqlStatement.Statement q qidEnc decoder True
  where
    q = "SELECT * FROM World WHERE (id = $1)"
    decoder = HasqlDec.singleRow $ Types.World <$> qidDec <*> qidDec

queryWorldById :: Pool -> Types.QId -> IO (Either Error Types.World)
queryWorldById pool wId = use pool (statement wId selectSingle)

queryWorldByIds :: Pool -> [Types.QId] -> IO (Either Error [Types.World])
queryWorldByIds _ [] = pure . pure $ mempty
queryWorldByIds pool wIds = use pool $ do
  forM wIds $ \wId -> statement wId selectSingle

updateSingle :: HasqlStatement.Statement (Types.QId, Types.QId) ()
updateSingle = HasqlStatement.Statement q encoder HasqlDec.noResult True
  where
    q = "UPDATE World SET randomNumber = $1 WHERE id = $2"
    encoder = contramap fst qidEnc <> contramap snd qidEnc

updateWorlds :: Pool -> [(Types.World, Types.QId)] -> IO (Either Error [Types.World])
updateWorlds _ [] = pure . pure $ mempty
updateWorlds pool wsUpdates = use pool $ do
  let ws = fmap updateW wsUpdates
  forM_ wsUpdates $ \(w, wNum) -> do
    statement (Types.wId w, wNum) updateSingle
  return ws
  where
    updateW (w, wNum) = w {Types.wRandomNumber = wNum}

-------------------------------------------------------------------------------

-- * Fortunes

selectFortunes :: HasqlStatement.Statement () [Types.Fortune]
selectFortunes = HasqlStatement.Statement q encoder decoder True
  where
    q = "SELECT * FROM Fortune"
    encoder = HasqlEnc.noParams
    -- TODO: investigate whether 'rowList' is worth the more expensive 'cons'.
    decoder = HasqlDec.rowList $ Types.Fortune <$> qidDec <*> HasqlDec.column (HasqlDec.nonNullable HasqlDec.text)
{-# INLINE selectFortunes #-}

queryFortunes :: Pool -> IO (Either Error [Types.Fortune])
queryFortunes pool = use pool (statement () selectFortunes)
