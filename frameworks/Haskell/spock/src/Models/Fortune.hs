{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Fortune
    ( Fortune(..)
    , fetchFortunes
    ) where

import           Data.Aeson
import           Data.Monoid                        ((<>))
import           Data.Ord
import qualified Data.Text                          as T
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics


data Fortune = Fortune
    { _idF      :: !Integer
    , _messageF :: !T.Text
    } deriving (Show, Generic)

-- | JSON serialization
instance ToJSON Fortune where
    toEncoding f =
        pairs (  "id"      .= _idF f
              <> "message" .= _messageF f
              )
    {-# INLINE toEncoding #-}

-- | Transforming a database row into a World datatype.
instance FromRow Fortune where
    fromRow = Fortune <$> field <*> field
    {-# INLINE fromRow #-}

-- | For sorting purposes
instance Eq Fortune where
    (==) fa fb =
        _idF fa      == _idF fb
     && _messageF fa == _messageF fb
    {-# INLINE (==) #-}

instance Ord Fortune where
    compare = comparing _messageF
    {-# INLINE compare #-}

fetchFortunes :: PG.Connection -> IO [Fortune]
fetchFortunes c = PG.query_ c "SELECT id, message FROM Fortune"
{-# INLINE fetchFortunes #-}
