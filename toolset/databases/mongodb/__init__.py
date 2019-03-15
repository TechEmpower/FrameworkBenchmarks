import pymongo
import traceback

from colorama import Fore
from toolset.utils.output_helper import log

class Mongodb:
    description = "A popular document-store database"

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
            connection = pymongo.MongoClient(
                host=config.database_host)
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
            connection = pymongo.MongoClient(host=config.database_host)
            db = connection.hello_world
            db.world.find()
            db.close()
            return True
        except:
            return False
