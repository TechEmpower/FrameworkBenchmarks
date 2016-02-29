{-# LANGUAGE OverloadedStrings #-}

module Models.World
    ( World(..)
    , fetchWorldById
    , getRandomWorld
    , fetchRandomWorldsAsync
    , updateWorldsRandomAsync
    ) where

import           Control.Concurrent.Async
import           Data.Aeson
import           Data.Maybe
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           System.Random


data World = World
    { _idW           :: !Integer
    , _randomNumberW :: !Integer
    } deriving (Show)

-- | JSON serialization
instance ToJSON World where
    toJSON w = object
        [ "id"            .= _idW w
        , "randomNumber"  .= _randomNumberW w
        ]

-- | Transforming a database row into a World datatype.
instance FromRow World where
    fromRow = World <$> field <*> field

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

-- | Get n random Worlds in a concurrent way.
fetchRandomWorldsAsync :: Int -> PG.Connection -> IO [World]
fetchRandomWorldsAsync n c = do
    maybes <- mapConcurrently (\_ -> getRandomWorld c) [1..n]
    return $ catMaybes maybes
{-# INLINE fetchRandomWorldsAsync #-}

-- | Update a World with a random number
updateWorldRandom :: PG.Connection -> World -> IO World
updateWorldRandom c (World _id _) = do
    i <- randomRIO (1, 10000)
    _ <- PG.execute c "UPDATE World SET randomNumber = ? WHERE id = ?" (i, _id)
    return $ World _id i
{-# INLINE updateWorldRandom #-}

-- | Update a bunch of Worlds in a concurrent way.
updateWorldsRandomAsync :: [World] -> PG.Connection -> IO [World]
updateWorldsRandomAsync ws c =
    mapConcurrently (updateWorldRandom c) ws
{-# INLINE updateWorldsRandomAsync #-}
