-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Prelude
import Database.Persist.MySQL (MySQLConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Yaml

type PersistConfig = MySQLConf

data Extra = Extra
    { 
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = return Extra
