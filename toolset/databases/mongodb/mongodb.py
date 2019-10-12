import pymongo
import traceback

from colorama import Fore
from toolset.utils.output_helper import log
from toolset.databases.abstract_database import AbstractDatabase

class Database(AbstractDatabase):

    @classmethod
    def get_connection(cls, config):
        return pymongo.MongoClient(host=config.database_host)

    @classmethod
    def get_current_world_table(cls, config):
        '''
        Return a JSON object containing all 10,000 World items as they currently
        exist in the database. This is used for verifying that entries in the
        database have actually changed during an Update verification test.
        '''
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
    def get_queries(cls, config, tbl_name):
        db = cls.get_connection(config)
        status = db.admin.command(pymongo.son_manipulator.SON([('serverStatus', 1)]))
        return int(status["opcounters"]["query"])

    @classmethod
    def get_rows(cls, config, tbl_name):
        # rows doesn't make sense with Mongo
        return cls.get_queries(config)

    @classmethod
    def get_rows_updated(cls, config):
        db = cls.get_connection(config)
        status = db.admin.command(pymongo.son_manipulator.SON([('serverStatus', 1)]))
        return int(status["opcounters"]["update"])

    @classmethod
    def reset_cache(cls, config):
        db = cls.get_connection(config)
        db.admin.command({"planCacheClear": "world"})
        db.admin.command({"planCacheClear": "fortune"})
        
