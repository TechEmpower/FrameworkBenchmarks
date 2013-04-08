module Foundation where

import Prelude
import Yesod
import Yesod.Default.Config
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (Extra (..))

data App = App
    { settings :: AppConfig DefaultEnv Extra
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings
    logLevel _ = LevelError

instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod
