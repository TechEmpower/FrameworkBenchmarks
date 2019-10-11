import json
import MySQLdb
import traceback

from colorama import Fore
from toolset.utils.output_helper import log
from toolset.databases.abstract_database import AbstractDatabase


class Database(AbstractDatabase):

    @staticmethod
    def get_connection(config):
        return MySQLdb.connect(config.database_host, "benchmarkdbuser",
                                 "benchmarkdbpass", "hello_world")

    @staticmethod
    def get_current_world_table(config):
        '''
        Return a JSON object containing all 10,000 World items as they currently
        exist in the database. This is used for verifying that entries in the
        database have actually changed during an Update verification test.
        '''
        results_json = []

        try:
            db = Database.get_connection(config)
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
            db = Database.get_connection(config)
            cursor = db.cursor()
            cursor.execute("SELECT 1")
            cursor.fetchall()
            db.close()
            return True
        except:
            return False

    @staticmethod
    def get_queries(config):
        db = Database.get_connection(config)
        cursor = db.cursor()
        cursor.execute("Show session status like 'Queries'")
        record = cursor.fetchone()
        return record[1]

    @staticmethod
    def get_rows(config):
        db = Database.get_connection(config)
        cursor = db.cursor()
        cursor.execute("show session status like 'Innodb_rows_read'")
        record = cursor.fetchone()
        return record[1]

    @staticmethod
    def reset_cache(config):
        #No more in Mysql 8.0
        #cursor = self.db.cursor()
        #cursor.execute("RESET QUERY CACHE")
        #self.db.commit()
        return
