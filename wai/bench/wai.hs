{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.=), object, encode)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.Warp as W
import Program.Mighty.Network (listenSocket)

main :: IO ()
main = do
    s <- listenSocket "8000" 2048
    W.runSettingsSocket settings s app
  where
    settings = W.setOnException (\_ _ -> return ()) W.defaultSettings
    app _ = return response
    response = responseLBS status200 ct json
    ct = [("Content-Type", "application/json")]
    json = encode $ object ["message" .= ("Hello, World!" :: Text)]
