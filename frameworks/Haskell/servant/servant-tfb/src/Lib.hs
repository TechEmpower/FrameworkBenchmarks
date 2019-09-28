{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

#if defined(DB_BEAM) || defined(DB_PSQL_SIMPLE)
#define HAS_DB 1
#endif

module Lib (
    main,
    -- * Exports to hide warnings
    withSMGen,
    ) where

import           Control.Concurrent         (myThreadId, threadCapability)
import           Control.Concurrent.MVar    (MVar, modifyMVarMasked, newMVar)
import           Data.Aeson                 (ToJSON (..), object, pairs, (.=))
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import qualified Data.Vector                as V
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.Random.SplitMix     as SM

#ifdef HAS_DB
import           Data.Int                   (Int32)
import           Data.Pool                  (Pool, createPool, withResource)

#endif

#ifdef DB_BEAM
import           Database.Beam
import           Database.Beam.Postgres
#endif

#ifdef DB_PSQL_SIMPLE
import           Control.Monad.IO.Class     (MonadIO (..))
import           Database.PostgreSQL.Simple
#endif

-------------------------------------------------------------------------------
-- API Definition
-------------------------------------------------------------------------------

data Routes route = Routes
    { routePlaintext :: route :- "plaintext" :> Get '[PlainText] Text
    , routeJson      :: route :- "json"      :> Get '[JSON]      JsonData
#ifdef HAS_DB
    , routeDb        :: route :- "db"        :> Get '[JSON]      World
#endif
    }
  deriving (Generic)

serverRoutes :: Ctx -> Routes AsServer
serverRoutes _ctx = Routes
    { routePlaintext = handlerPlaintext
    , routeJson      = handlerJson
#ifdef HAS_DB
    , routeDb        = handlerDb _ctx
#endif
    }

app :: Ctx -> Application
app = genericServe . serverRoutes

-- | entry point
main
    :: Int  -- ^ number of capabilities
    -> IO ()
main _cap = do
    -- create 32 randon number generators
    rng <- V.replicateM 32 (SM.newSMGen >>= newMVar)

#ifdef HAS_DB
    db <- createPool
        (connect dbConnectInfo)
        close
        _cap
        0.5
        512
#endif

    let ctx = Ctx { ctxRng = rng
#ifdef HAS_DB
                  , ctxDb  = db
#endif
                  }

    putStrLn "Servant is ready to serve you"
    Warp.run 7041 $ app ctx
  where
#ifdef HAS_DB
    dbConnectInfo :: ConnectInfo
    dbConnectInfo = ConnectInfo
        { connectHost     = "tfb-database"
        , connectPort     = 5432
        , connectUser     = "benchmarkdbuser"
        , connectPassword = "benchmarkdbpass"
        , connectDatabase = "hello_world"
        }
#endif

-------------------------------------------------------------------------------
-- Execution context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxRng :: !(Vector (MVar SM.SMGen))
#ifdef HAS_DB
    , ctxDb  :: !(Pool Connection)
#endif
    }

#ifdef HAS_DB
withConnection :: Ctx -> (Connection -> IO r) -> IO r
withConnection ctx = withResource (ctxDb ctx)
#endif

withSMGen :: Ctx -> (SM.SMGen -> IO r) -> IO r
withSMGen ctx k = do
    tid <- myThreadId
    (cap, _) <- threadCapability tid
    gen <- modifyMVarMasked (ctxRng ctx V.! mod cap 32) $ return . SM.splitSMGen
    k gen

-------------------------------------------------------------------------------
-- Test 1: JSON serialization
-------------------------------------------------------------------------------

newtype JsonData = JsonData Text

instance ToJSON JsonData where
    toEncoding (JsonData t) = pairs ("message" .= t)
    toJSON     (JsonData t) = object [ "message" .= t ]


handlerJson :: Handler JsonData
handlerJson = return $ JsonData "Hello, World!"

-------------------------------------------------------------------------------
-- Test 2: Single database query
-------------------------------------------------------------------------------

#ifdef DB_BEAM
handlerDb :: Ctx -> Handler World
handlerDb ctx = liftIO $
    withConnection ctx $ \conn ->
    withSMGen ctx $ \gen -> do
        -- generate random id, first [0, 10000), then 'succ' to [1,10000]
        let (randomId', _) = SM.bitmaskWithRejection32 10000 gen
        let randomId :: Int32
            randomId = succ (fromIntegral randomId')

        ws <- runBeamPostgres conn $ runSelectReturningList $
            lookup_ (tfbWorld tfbDb) (WorldId randomId)

        case ws of
            (w:_) -> return w
            []    -> return $ World 0 0
#endif

#ifdef DB_PSQL_SIMPLE
handlerDb :: Ctx -> Handler World
handlerDb ctx = liftIO $
    withConnection ctx $ \conn ->
    withSMGen ctx $ \gen -> do
        -- generate random id, first [0, 10000), then 'succ' to [1,10000]
        let (randomId', _) = SM.bitmaskWithRejection32 10000 gen
        let randomId :: Int32
            randomId = succ (fromIntegral randomId')

        ws <- query conn "SELECT id, randomnumber FROM World where id = ?" (Only randomId)

        case ws of
            (w:_) -> return w
            []    -> return $ World 0 0
#endif

-------------------------------------------------------------------------------
-- Test 3: Multiple database query
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test 4: Fortunes
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test 5: Updates
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Test 6: Plaintext endpoint
-------------------------------------------------------------------------------

handlerPlaintext :: Handler Text
handlerPlaintext = return "Hello, World!"

-------------------------------------------------------------------------------
-- beam
-------------------------------------------------------------------------------

#ifdef DB_BEAM
-- | World table.
data WorldT f = World
    { worldId           :: Columnar f Int32
    , worldRandomNumber :: Columnar f Int32
    }
  deriving (Generic, Beamable)

instance Table WorldT where
   data PrimaryKey WorldT f = WorldId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey = WorldId . worldId

type World = WorldT Identity
-- type WorldId = PrimaryKey WorldT Identity

deriving instance Eq World
deriving instance Show World

-- Database definition.
newtype TfbDb f = TfbDb
    { tfbWorld :: f (TableEntity WorldT)
    }
 deriving (Generic, Database Postgres)

tfbDb :: DatabaseSettings Postgres TfbDb
tfbDb = defaultDbSettings `withDbModification` modification where
    modification = (dbModification :: DatabaseModification (DatabaseEntity Postgres TfbDb) Postgres TfbDb)
        { tfbWorld = modifyEntityName (\_ -> "World") <> modifyTableFields tableModification
            { worldId           = "id"
            , worldRandomNumber = "randomnumber"
            }
        }
#endif

-------------------------------------------------------------------------------
-- postgresql-simple
-------------------------------------------------------------------------------

#ifdef DB_PSQL_SIMPLE
data World = World
    { worldId           :: Int32
    , worldRandomNumber :: Int32
    }
  deriving (Eq, Show, Generic, FromRow)
#endif

-------------------------------------------------------------------------------
-- DB common
-------------------------------------------------------------------------------

#ifdef HAS_DB
instance ToJSON World where
    toEncoding (World i rn) = pairs ("id" .= i <> "randomNumber" .= rn)
    toJSON     (World i rn) = object ["id" .= i, "randomNumber" .= rn]
#endif
