module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

-- Generator Marker
instance CanRoute FrameworkBenchmarksController where
    parseRoute' = (string "/json" <* endOfInput >> pure JsonAction)
        <|> (string "/plaintext" <* endOfInput >> pure PlaintextAction)
        <|> (string "/db" <* endOfInput >> pure DbAction)
        <|> (string "/fortunes" <* endOfInput >> pure FortuneAction)
        <|> (string "/query" <* endOfInput >> pure QueryAction)
        <|> (string "/update" <* endOfInput >> pure UpdatesAction)

instance HasPath FrameworkBenchmarksController