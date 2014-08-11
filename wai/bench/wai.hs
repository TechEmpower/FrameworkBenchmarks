{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (runInUnboundThread)
import Data.Aeson ((.=), object, encode)
import Data.Streaming.Network (bindPortTCP)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.Warp as W

main :: IO ()
main = runInUnboundThread $ do
    s <- bindPortTCP 8000 "*"
    W.runSettingsSocket settings s app
  where
    settings = W.setOnException (\_ _ -> return ()) W.defaultSettings
    app _ = return response
    response = responseLBS status200 ct json
    ct = [("Content-Type", "application/json")]
    json = encode $ object ["message" .= ("Hello, World!" :: Text)]
