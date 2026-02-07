{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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

import Control.Exception (catch, try)
import Control.Monad (forM)
import Data.Bifunctor qualified as Bi
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Either qualified as Either
import Data.Pool qualified as Pool
import Database.PostgreSQL.Simple (SomePostgreSqlException)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import System.IO.Error qualified as Error
import TFB.Types qualified as Types

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

instance FromRow Types.World where
  fromRow = Types.World <$> field <*> field

instance FromRow Types.Fortune where
  fromRow = Types.Fortune <$> field <*> field

type Connection = PG.Connection

type Pool = Pool.Pool Connection

data Error
  = DbError ByteString
  | DbErrors [ByteString]
  | NotFound
  deriving (Show)

connect :: Config -> IO Connection
connect c = catch (PG.connect pgc) failError
  where
    failError :: PG.SomePostgreSqlException -> IO a
    failError = Error.ioError . Error.userError . show
    pgc =
      PG.defaultConnectInfo
        { PG.connectHost = configHost c,
          PG.connectDatabase = BSC.unpack $ configName c,
          PG.connectUser = BSC.unpack $ configUser c,
          PG.connectPassword = BSC.unpack $ configPass c
        }

close :: Connection -> IO ()
close = PG.close

mkPool :: Config -> IO Pool
mkPool c =
  Pool.newPool $
    Pool.setNumStripes (Just $ configStripes c) $
      Pool.defaultPoolConfig
        (connect c)
        close
        0.5
        (configPoolSize c)

-------------------------------------------------------------------------------

-- * World

queryWorldByIdInner :: Types.QId -> Connection -> IO (Either Error Types.World)
queryWorldByIdInner wId conn = do
  let query = PG.query conn "SELECT * FROM World WHERE id = ?" (PG.Only wId :: PG.Only Types.QId) :: IO [Types.World]
  res <- try @SomePostgreSqlException query
  pure $ Either.either (Left . DbError . BSC.pack . show) mkW res
  where
    mkW [] = Left NotFound
    mkW (w : _) = pure w

queryWorldById :: Pool -> Types.QId -> IO (Either Error Types.World)
queryWorldById dbPool wId = Pool.withResource dbPool (queryWorldByIdInner wId)

queryWorldByIds :: Pool -> [Types.QId] -> IO (Either Error [Types.World])
queryWorldByIds dbPool wIds = Pool.withResource dbPool $ \conn -> do
  rows <- forM wIds $ \wId -> queryWorldByIdInner wId conn
  let (errs, rowsList) = Either.partitionEithers rows
  return $ case errs of
    [] -> pure rowsList
    _ ->
      Left . DbErrors $
        map
          ( \case
              DbError e -> e
              _ -> error "Unexpected error"
          )
          errs

updateWorlds :: Pool -> [(Types.World, Int)] -> IO (Either Error [Types.World])
updateWorlds dbPool wsUpdates = Pool.withResource dbPool $ \conn -> do
  let worlds = Bi.first Types.wId <$> wsUpdates
  res <-
    try @SomePostgreSqlException $
      PG.executeMany
        conn
        [sql| UPDATE World 
              SET randomNumber = upd.rnd 
              FROM (VALUES (?,?)) as upd(wid,rnd) 
              WHERE World.id = upd.wid |]
        worlds
  _ <- case res of
    Left e -> print e
    Right _ -> return ()
  pure $ Bi.bimap (DbError . BSC.pack . show) (const $ map (uncurry Types.World) worlds) res

-------------------------------------------------------------------------------

-- * Fortunes

queryFortunes :: Pool -> IO (Either Error [Types.Fortune])
queryFortunes dbPool = Pool.withResource dbPool $ \conn -> do
  let query = PG.query_ conn "SELECT * FROM Fortune" :: IO [Types.Fortune]
  res <- try @SomePostgreSqlException query
  pure $ Bi.first (DbError . BSC.pack . show) res
