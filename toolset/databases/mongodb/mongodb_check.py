import pymongo
from toolset.databases.database_check import DatabaseCheck

class MongoDbCheck(DatabaseCheck):
    def connect(self):
        self.db = pymongo.MongoClient(
                    host=self.config.database_host)

    def get_queries(self):
        status = self.db.admin.command(pymongo.son_manipulator.SON([('serverStatus', 1)]))
        return int(status["opcounters"]["query"])

    def get_rows(self):
        return self.get_queries()

    def reset_cache(self):
        self.db.admin.command({"planCacheClear": "world"})
        self.db.admin.command({"planCacheClear": "fortune"})
