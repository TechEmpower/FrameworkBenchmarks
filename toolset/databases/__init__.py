from toolset.databases.mongodb import Mongodb
from toolset.databases.mysql import Mysql
from toolset.databases.postgres import Postgres

databases = {
    "mongodb": Mongodb,
    "mysql": Mysql,
    "postgres": Postgres
}
