{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import Blaze.ByteString.Builder (copyByteString)
import Control.Concurrent (runInUnboundThread)
import Data.Aeson ((.=), object, encode)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (responseBuilder)
import qualified Network.Wai.Handler.Warp as W

main :: IO ()
main =
    runInUnboundThread $ W.runSettings settings app
  where
    settings = W.setPort 8000
             $ W.setOnException (\_ _ -> return ()) W.defaultSettings
    app _ respond = respond response
    !response = responseBuilder status200 ct json
    ct = [("Content-Type", "application/json")]
    !json = copyByteString
          $ L.toStrict
          $ encode
          $ object ["message" .= ("Hello, World!" :: Text)]
