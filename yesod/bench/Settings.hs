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
