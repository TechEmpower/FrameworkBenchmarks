import json
import MySQLdb
import traceback

from colorama import Fore
from toolset.utils.output_helper import log

class Mysql:
    description = "One of the most popular databases around the web and in TFB"

    @staticmethod
    def get_current_world_table(config):
        '''
        Return a JSON object containing all 10,000 World items as they currently
        exist in the database. This is used for verifying that entries in the
        database have actually changed during an Update verification test.
        '''
        results_json = []

        try:
            db = MySQLdb.connect(config.database_host,
                                 "benchmarkdbuser", "benchmarkdbpass",
                                 "hello_world")
            cursor = db.cursor()
            cursor.execute("SELECT * FROM World")
            results = cursor.fetchall()
            results_json.append(json.loads(json.dumps(dict(results))))
            db.close()
        except Exception:
            tb = traceback.format_exc()
            log("ERROR: Unable to load current MySQL World table.",
                color=Fore.RED)
            log(tb)

        return results_json

    @staticmethod
    def test_connection(config):
        try:
            db = MySQLdb.connect(config.database_host, "benchmarkdbuser",
                                 "benchmarkdbpass", "hello_world")
            cursor = db.cursor()
            cursor.execute("SELECT 1")
            cursor.fetchall()
            db.close()
            return True
        except:
            return False
