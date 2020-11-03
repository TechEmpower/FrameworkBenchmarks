module Main where
import IHP.Prelude

import Config
import qualified IHP.Server
import IHP.RouterSupport
import IHP.FrameworkConfig
import Web.FrontController
import Web.Types

instance FrontController RootApplication where
    controllers = [
            mountFrontController WebApplication
        ]

main :: IO ()
main = IHP.Server.run
