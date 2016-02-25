{-# LANGUAGE OverloadedStrings #-}

module Models.Fortune
    ( Fortune(..)
    , fetchFortunes
    ) where

import           Data.Aeson
import           Data.Ord
import qualified Data.Text                          as T
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow


data Fortune = Fortune
    { _idF      :: Integer
    , _messageF :: T.Text
    } deriving (Show)

-- | JSON serialization
instance ToJSON Fortune where
    toJSON f = object
        [ "id"       .= _idF f
        , "message"  .= _messageF f
        ]

-- | Transforming a database row into a World datatype.
instance FromRow Fortune where
    fromRow = Fortune <$> field <*> field

-- | For sorting purposes
instance Eq Fortune where
    (==) fa fb =
        _idF fa      == _idF fb
     && _messageF fa == _messageF fb

instance Ord Fortune where
    compare = comparing _messageF


fetchFortunes :: PG.Connection -> IO [Fortune]
fetchFortunes c = PG.query_ c "SELECT id, message FROM Fortune"
