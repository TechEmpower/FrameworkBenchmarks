import json
import psycopg2
import traceback

from colorama import Fore
from toolset.utils.output_helper import log

class Database:

    @staticmethod
    def get_current_world_table(config):
        '''
        Return a JSON object containing all 10,000 World items as they currently
        exist in the database. This is used for verifying that entries in the
        database have actually changed during an Update verification test.
        '''
        results_json = []

        try:
            db = psycopg2.connect(
                host=config.database_host,
                port="5432",
                user="benchmarkdbuser",
                password="benchmarkdbpass",
                database="hello_world")
            cursor = db.cursor()
            cursor.execute("SELECT * FROM \"World\"")
            results = cursor.fetchall()
            results_json.append(json.loads(json.dumps(dict(results))))
            cursor = db.cursor()
            cursor.execute("SELECT * FROM \"world\"")
            results = cursor.fetchall()
            results_json.append(json.loads(json.dumps(dict(results))))
            db.close()
        except Exception:
            tb = traceback.format_exc()
            log("ERROR: Unable to load current Postgres World table.",
                color=Fore.RED)
            log(tb)

        return results_json

    @staticmethod
    def test_connection(config):
        try:
            db = psycopg2.connect(
                host=config.database_host,
                port="5432",
                user="benchmarkdbuser",
                password="benchmarkdbpass",
                database="hello_world")
            cursor = db.cursor()
            cursor.execute("SELECT 1")
            cursor.fetchall()
            db.close()
            return True
        except:
            return False
