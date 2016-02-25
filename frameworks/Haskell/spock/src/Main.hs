{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async           (mapConcurrently)
import           Control.Monad.IO.Class
import           Data.Aeson                         hiding (json)
import           Data.Maybe                         (catMaybes, listToMaybe)
import qualified Data.Text                          as T
import qualified Database.PostgreSQL.Simple         as PG
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Exts
import           Network.HTTP.Types.Status
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


fetchWorldById :: Int -> PG.Connection -> IO (Maybe World)
fetchWorldById i c =
    listToMaybe <$> PG.query c
        "SELECT id, randomNumber FROM World WHERE id = (?)"
        (PG.Only i)


data Fortune = Fortune
    { _idF      :: Integer
    , _messageF :: T.Text
    } deriving (Show)


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
            -- | Test 1: JSON serialization
        do  get "json" $
                json $ Object (fromList [("message", "Hello, World!")])
            -- | Test 2: Single database query
            get "db" $
                do  rand <- liftIO $ randomRIO (1, 10000)
                    maybeWorld <- runQuery $ fetchWorldById rand
                    case maybeWorld of
                      Just w  -> json w
                      Nothing -> setStatus status404 >> text "World not found."
            -- | Test 3: Multiple database queries
            get "queries" $
                do  queries <- max 1 . min 500 <$> param' "queries"
                    let runSQL core = runSpockIO core . runQuery . fetchWorldById =<< randomRIO (1, 10000)
                    spockCore <- getSpockHeart
                    maybeWorlds <- liftIO $ mapConcurrently (\_ -> runSQL spockCore) ([1..queries] :: [Int])
                    json $ catMaybes maybeWorlds
            -- | Test 4: Fortunes
            get "fortune" $
                do  liftIO $ putStrLn "test"
            -- | Test 6: Plain text
            get "plaintext" $
                text "Hello, World!"
