module Web.FrontController where
import IHP.RouterPrelude
import IHP.ControllerSupport
import Generated.Types
import Web.Types

-- Controller Imports
import Web.Controller.FrameworkBenchmarks
import IHP.Welcome.Controller

instance FrontController WebApplication where
    controllers = 
        [ parseRoute @FrameworkBenchmarksController
        -- Generator Marker
        ]

instance InitControllerContext WebApplication
