import pymongo
import traceback

from colorama import Fore
from toolset.utils.output_helper import log
from toolset.databases.abstract_database import AbstractDatabase

class Database(AbstractDatabase):

    @classmethod
    def get_connection(cls, config):
        return pymongo.MongoClient(host = config.database_host)

    @classmethod
    def get_current_world_table(cls, config):
        results_json = []

        try:
            worlds_json = {}
            print("DATABASE_HOST: %s" % config.database_host)
            connection = cls.get_connection(config)
            db = connection.hello_world
            for world in db.world.find():
                if "randomNumber" in world:
                    if "id" in world:
                        worlds_json[str(int(world["id"]))] = int(
                            world["randomNumber"])
                    elif "_id" in world:
                        worlds_json[str(int(world["_id"]))] = int(
                            world["randomNumber"])
            results_json.append(worlds_json)
            connection.close()
        except Exception:
            tb = traceback.format_exc()
            log("ERROR: Unable to load current MongoDB World table.",
                color=Fore.RED)
            log(tb)

        return results_json

    @classmethod
    def test_connection(cls, config):
        try:
            connection = cls.get_connection(config)
            db = connection.hello_world
            db.world.find()
            db.close()
            return True
        except:
            return False

    @classmethod
    def get_queries(cls, config):
        co = cls.get_connection(config)
        status = co.admin.command(pymongo.son_manipulator.SON([('serverStatus', 1)]))
        return int(status["opcounters"]["query"]) + int(status["opcounters"]["update"]) #get_queries returns all the queries

    @classmethod
    def get_rows(cls, config):
        co = cls.get_connection(config)
        status = co.admin.command(pymongo.son_manipulator.SON([('serverStatus', 1)]))
        return int(status["opcounters"]["query"]) * cls.get_rows_per_query(co)

    @classmethod
    def get_rows_updated(cls, config):
        co = cls.get_connection(config)
        status = co.admin.command(pymongo.son_manipulator.SON([('serverStatus', 1)]))
        return int(status["opcounters"]["update"]) * cls.get_rows_per_query(co)

    @classmethod
    def reset_cache(cls, config):
        co = cls.get_connection(config)
        co.admin.command({"planCacheClear": "world"})
        co.admin.command({"planCacheClear": "fortune"})

    @classmethod
    def get_rows_per_query(cls, co):
        rows_per_query = 1
        if cls.tbl_name == "fortune":
            rows_per_query = co["hello_world"][cls.tbl_name].count_documents({})
        return rows_per_query
