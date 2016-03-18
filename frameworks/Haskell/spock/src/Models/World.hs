{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.World
    ( World(..)
    , fetchWorldById
    , getRandomWorld
    , updateWorldRandom
    ) where

import           Data.Aeson
import           Data.Maybe
import           Data.Monoid                        ((<>))
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics
import           System.Random


data World = World
    { _idW           :: !Integer
    , _randomNumberW :: !Integer
    } deriving (Show, Generic)

-- | JSON serialization
instance ToJSON World where
    toEncoding w =
        pairs (  "id"            .= _idW w
              <> "randomNumber"  .= _randomNumberW w
              )
    {-# INLINE toEncoding #-}

-- | Transforming a database row into a World datatype.
instance FromRow World where
    fromRow = World <$> field <*> field
    {-# INLINE fromRow #-}

-- | Get a World by Id, this will return a Just World, or Nothing
-- if the id is not in the database.
fetchWorldById :: Int -> PG.Connection -> IO (Maybe World)
fetchWorldById i c =
    listToMaybe <$> PG.query c
        "SELECT id, randomNumber FROM World WHERE id = ?"
        (PG.Only i)
{-# INLINE fetchWorldById #-}

-- | Get a random World from the database. For the tests
-- the id must be bound between 1-10000
getRandomWorld :: PG.Connection -> IO (Maybe World)
getRandomWorld c = do
    i <- randomRIO (1, 10000)
    fetchWorldById i c
{-# INLINE getRandomWorld #-}

-- | Update a World with a random number
updateWorldRandom :: World -> PG.Connection -> IO World
updateWorldRandom (World _id _) c = do
    i <- randomRIO (1, 10000)
    _ <- PG.execute c "UPDATE World SET randomNumber = ? WHERE id = ?" (i, _id)
    return $ World _id i
{-# INLINE updateWorldRandom #-}
