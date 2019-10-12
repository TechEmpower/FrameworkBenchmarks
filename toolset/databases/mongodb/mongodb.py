import pymongo
import traceback

from colorama import Fore
from toolset.utils.output_helper import log
from toolset.databases.abstract_database import AbstractDatabase

class Database(AbstractDatabase):

    @staticmethod
    def get_connection(config):
        return pymongo.MongoClient(host=config.database_host)

    @staticmethod
    def get_current_world_table(config):
        '''
        Return a JSON object containing all 10,000 World items as they currently
        exist in the database. This is used for verifying that entries in the
        database have actually changed during an Update verification test.
        '''
        results_json = []

        try:
            worlds_json = {}
            print("DATABASE_HOST: %s" % config.database_host)
            connection = Database.get_connection(config)
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

    @staticmethod
    def test_connection(config):
        try:
            connection = Database.get_connection(config)
            db = connection.hello_world
            db.world.find()
            db.close()
            return True
        except:
            return False

    @staticmethod
    def get_queries(config, tbl_name):
        db = Database.get_connection(config)
        status = db.admin.command(pymongo.son_manipulator.SON([('serverStatus', 1)]))
        return int(status["opcounters"]["query"])

    @staticmethod
    def get_rows(config, tbl_name):
        return Database.get_queries()

    @staticmethod
    def reset_cache(config):
        db = Database.get_connection(config)
        db.admin.command({"planCacheClear": "world"})
        db.admin.command({"planCacheClear": "fortune"})
        
