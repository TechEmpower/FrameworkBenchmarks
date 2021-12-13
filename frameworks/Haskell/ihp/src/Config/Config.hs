module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Mail.Types

config :: ConfigBuilder
config = do
    option Production
    option (AppHostname "localhost")
    option (RequestLoggerMiddleware $ \application -> application)
    option (DBPoolIdleTime 10)
    option (DBPoolMaxConnections 512)