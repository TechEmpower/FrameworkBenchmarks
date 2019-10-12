import json
import MySQLdb
import traceback

from colorama import Fore
from toolset.utils.output_helper import log
from toolset.databases.abstract_database import AbstractDatabase


class Database(AbstractDatabase):

    @classmethod
    def get_connection(cls, config):
        return MySQLdb.connect(config.database_host, "benchmarkdbuser",
                                 "benchmarkdbpass", "hello_world")

    @classmethod
    def get_current_world_table(cls, config):
        '''
        Return a JSON object containing all 10,000 World items as they currently
        exist in the database. This is used for verifying that entries in the
        database have actually changed during an Update verification test.
        '''
        results_json = []

        try:
            db = cls.get_connection(config)
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

    @classmethod
    def test_connection(cls, config):
        try:
            db = cls.get_connection(config)
            cursor = db.cursor()
            cursor.execute("SELECT 1")
            cursor.fetchall()
            db.close()
            return True
        except:
            return False

    @classmethod
    def get_queries(cls, config):
        db = cls.get_connection(config)
        cursor = db.cursor()
        cursor.execute("Show session status like 'Queries'")
        record = cursor.fetchone()
        return record[1]

    @classmethod
    def get_rows(cls, config):
        db = cls.get_connection(config)
        cursor = db.cursor()
        cursor.execute("show session status like 'Innodb_rows_read'")
        record = cursor.fetchone()
        return int(record[1] * 1.01) #Mysql lowers the number of rows read

    @classmethod
    def get_rows_updated(cls, config):
        db = cls.get_connection(config)
        cursor = db.cursor()
        cursor.execute("show session status like 'Innodb_rows_updated'")
        record = cursor.fetchone()
        return int(record[1] * 1.01) #Mysql lowers the number of rows updated

    @classmethod
    def reset_cache(cls, config):
        #No more in Mysql 8.0
        #cursor = self.db.cursor()
        #cursor.execute("RESET QUERY CACHE")
        #self.db.commit()
        return
