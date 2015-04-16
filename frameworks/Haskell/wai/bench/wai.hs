{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import Blaze.ByteString.Builder (copyByteString)
import Control.Concurrent (runInUnboundThread)
import Data.Aeson ((.=), object, encode)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Network.HTTP.Types (status200, status404)
import Network.Wai (responseBuilder, rawPathInfo)
import qualified Network.Wai.Handler.Warp as W

main :: IO ()
main =
    runInUnboundThread $ W.runSettings settings app
  where
    settings = W.setPort 8000
             $ W.setOnException (\_ _ -> return ()) W.defaultSettings
    app request respond = case rawPathInfo request of
        "/json" -> respond responseJson
        "/plaintext" -> respond responsePlaintext
        _ -> respond $ responseBuilder status404 [] ""
    !responseJson = responseBuilder status200 ctJson json
    ctJson = [("Content-Type", "application/json")]
    !json = copyByteString
          $ L.toStrict
          $ encode
          $ object ["message" .= ("Hello, World!" :: Text)]
    !responsePlaintext = responseBuilder status200 ctPlaintext plaintext
    ctPlaintext = [("Content-type", "text/plain")]
    plaintext = "Hello, World!"
