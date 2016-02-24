{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Aeson                         hiding (json)
import           Data.Maybe                         (listToMaybe)
import qualified Data.Text                          as T
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Exts
import           System.Random
import           Web.Spock.Safe


data World = World
    { _idW           :: Integer
    , _randomNumberW :: Integer
    } deriving (Show)


instance ToJSON World where
    toJSON w = object
        [ "id"            .= _idW w
        , "randomNumber"  .= _randomNumberW w
        ]

instance FromRow World where
    fromRow = World <$> field <*> field

data Fortune = Fortune
    { _idF      :: Integer
    , _messageF :: T.Text
    } deriving (Show)


fetchWorldById :: Int -> PG.Connection -> IO (Maybe World)
fetchWorldById i c =
    listToMaybe <$> PG.query c
        "SELECT id, randomNumber FROM World WHERE id = (?)"
        (PG.Only i)


dbConn :: PoolOrConn PG.Connection
dbConn =
  PCConn (ConnBuilder
            (PG.connect
                PG.ConnectInfo
                    { PG.connectHost     = "localhost"
                    , PG.connectPort     = 5432
                    , PG.connectUser     = "benchmarkdbuser"
                    , PG.connectPassword = "benchmarkdbpass"
                    , PG.connectDatabase = "hello_world" })
            PG.close
            (PoolCfg 5 5 60))


main :: IO ()
main =
    runSpock 3000 $ spock (defaultSpockCfg Nothing dbConn ()) $
        do  get "json" $
                -- | Test 1: JSON serialization
                json $ Object (fromList [("message", "Hello, World!")])
            get "db" $ do
                -- | Test 2: Single database query
                rand <- liftIO $ randomRIO (1, 10000)
                maybeWorld <- runQuery $ fetchWorldById rand
                case maybeWorld of
                  Just w  -> json w
                  Nothing -> text "World not found."
            get "plaintext" $
                -- | Test 6: Plain text
                text "Hello, World!"
