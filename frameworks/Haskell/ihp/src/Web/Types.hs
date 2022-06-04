module Web.Types where
import IHP.Prelude
import qualified IHP.Controller.Session
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.ModelSupport
import Application.Helper.Controller
import IHP.ViewSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data FrameworkBenchmarksController
    = JsonAction
    | PlaintextAction
    | DbAction
    | QueryAction
    | FortuneAction
    | UpdatesAction
    deriving (Eq, Show, Data)
