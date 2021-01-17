module Web.Controller.FrameworkBenchmarks where

import Web.Controller.Prelude
import Web.View.FrameworkBenchmarks.Fortune
import System.Random
import qualified Control.Concurrent.Async as Async

instance ToJSON World where
  toJSON world = object [("id" .= get #id world), ("randomNumber" .= get #randomnumber world)]

instance ToJSON Fortune where
  toJSON fortune = object ["message" .= get #message fortune]

instance Controller FrameworkBenchmarksController where
    action JsonAction = do
        renderJson (toJSON (object ["message" .= ("Hello, World!" :: Text)]))
    
    action DbAction = do
        randomNumber :: Int <- randomRIO (1, 10000)
        randomWorld <- query @World |> findBy #id (Id randomNumber)
        renderJson (toJSON randomWorld)

    action QueryAction = do
        let queries = defaultParam (paramOrError @Int "queries") |> toBoundaries
        let fetchRandomWorld i = do
                randomWorldId :: Id World <- Id <$> randomRIO (1, 10000)
                fetch randomWorldId
        [1..queries]
            |> Async.mapConcurrently fetchRandomWorld
            >>= renderJson
        where
            defaultParam (Right a) = a
            defaultParam _ = 1
    
    action FortuneAction = do
        allFortunes :: [Fortune] <- query @Fortune |> fetch
        let newFortune :: Fortune = newRecord @Fortune |> set #message "Additional fortune added at request time."
        let compareFortunes :: Fortune -> Fortune -> Ordering = \f1 f2 -> compare (get #message f1) (get #message f2)
        let fortunes :: [Fortune] = sortBy compareFortunes (newFortune:allFortunes)
        renderHtml FortuneView { .. } >>= respondHtml

    action UpdatesAction = do
        let queries :: Int = (defaultParam $ paramOrError @Int "queries" ) |> toBoundaries
        let updateRandomWorld i = do
                randomWorldId :: Id World <- Id <$> randomRIO (1, 10000)
                newRandom :: Int <- randomRIO (1, 10000)
                world <- fetch randomWorldId
                world 
                    |> set #randomnumber newRandom
                    |> updateRecord
        [1..queries]
            |> Async.mapConcurrently updateRandomWorld
            >>= renderJson
        where
            defaultParam (Right a) = a
            defaultParam _ = 1
    
    action PlaintextAction = do
        renderPlain "Hello, World!"
    
toBoundaries n
    | n < 1 = 1
    | n > 500 = 500
    | otherwise = n