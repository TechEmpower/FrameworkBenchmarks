{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Text (Text)

import Control.Monad (replicateM_)
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as Warp
import System.Posix.Process (forkProcess)
import Data.Conduit.Network (bindPort)
import Network.Wai
import System.Environment (getArgs)

main :: IO ()
main = do
    socket <- bindPort 8001 "*"
    [cores, _] <- getArgs
    let run = Warp.runSettingsSocket Warp.defaultSettings
                { Warp.settingsPort = 8001
                , Warp.settingsHost = "*"
                , Warp.settingsOnException = const $ return ()
                } socket app
    replicateM_ (read cores - 1) $ forkProcess run
    run
  where
    app _ = return $ responseLBS
      status200 [("Content-Type", "application/json")] $
      encode $ object ["message" .= ("Hello, World!" :: Text)]
