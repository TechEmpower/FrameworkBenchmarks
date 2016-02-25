{-# LANGUAGE OverloadedStrings #-}

module Models.World
    ( World(..)
    , fetchWorldById
    ) where

import           Data.Aeson
import           Data.Maybe
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow


data World = World
    { _idW           :: Integer
    , _randomNumberW :: Integer
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
        "SELECT id, randomNumber FROM World WHERE id = (?)"
        (PG.Only i)
