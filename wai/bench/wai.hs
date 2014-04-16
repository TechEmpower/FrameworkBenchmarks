{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.=), object, encode)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.Warp as W

main :: IO ()
main = W.runSettings settings app
  where
    settings = W.setOnException (\_ _ -> return ())
             $ W.setHost "*"
             $ W.setPort 8000 W.defaultSettings
    app _ = return response
    response = responseLBS status200 ct json
    ct = [("Content-Type", "application/json")]
    json = encode $ object ["message" .= ("Hello, World!" :: Text)]
