import json
import MySQLdb
import traceback

from colorama import Fore
from toolset.utils.output_helper import log
from toolset.databases.abstract_database import AbstractDatabase


class Database(AbstractDatabase):

    margin = 1.0015

    @classmethod
    def get_connection(cls, config):
        return MySQLdb.connect(config.database_host, "benchmarkdbuser",
                                 "benchmarkdbpass", "hello_world")

    @classmethod
    def get_current_world_table(cls, config):
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
        #Returns an empty result!!!
        #cursor.execute("SELECT SUM(variable_value) as s FROM PERFORMANCE_SCHEMA.SESSION_STATUS where Variable_name in ('Com_select','Com_update')")
        #so use of this inelegant solution
        cursor.execute("Show global status like 'Com_select'")
        recordS = cursor.fetchone()
        cursor.execute("Show global status like 'Com_update'")
        recordU = cursor.fetchone()
        return int(recordS[1]) + int(recordU[1]) -1

    @classmethod
    def get_rows(cls, config):
        db = cls.get_connection(config)
        cursor = db.cursor()
        cursor.execute("""SELECT r.variable_value-u.variable_value FROM 
                        (SELECT variable_value FROM PERFORMANCE_SCHEMA.SESSION_STATUS where Variable_name like 'Innodb_rows_read') r,
                        (SELECT variable_value FROM PERFORMANCE_SCHEMA.SESSION_STATUS where Variable_name like 'Innodb_rows_updated') u""")
        record = cursor.fetchone()
        return int(int(record[0]) * cls.margin) #Mysql lowers the number of rows read

    @classmethod
    def get_rows_updated(cls, config):
        db = cls.get_connection(config)
        cursor = db.cursor()
        cursor.execute("show session status like 'Innodb_rows_updated'")
        record = cursor.fetchone()
        return int(int(record[1]) * cls.margin) #Mysql lowers the number of rows updated

    @classmethod
    def reset_cache(cls, config):
        #No more Cache in Mysql 8.0
        #cursor = self.db.cursor()
        #cursor.execute("RESET QUERY CACHE")
        #self.db.commit()
        return
