import json
import psycopg2
import traceback

from colorama import Fore
from toolset.utils.output_helper import log
from toolset.databases.abstract_database import AbstractDatabase

class Database(AbstractDatabase):

    @classmethod
    def get_connection(cls, config):
        db = psycopg2.connect(
                host=config.database_host,
                port="5432",
                user="benchmarkdbuser",
                password="benchmarkdbpass",
                database="hello_world")
        cursor = db.cursor()
        cursor.execute("CREATE EXTENSION IF NOT EXISTS pg_stat_statements")
        return db

    @classmethod
    def get_current_world_table(cls, config):
        results_json = []

        try:
            db = cls.get_connection(config)
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
        return cls.__exec_and_fetchone(config, "SELECT SUM(calls) FROM pg_stat_statements WHERE query ~* '[[:<:]]%s[[:>:]]'" % cls.tbl_name)

    @classmethod
    def get_rows(cls, config):
        return cls.__exec_and_fetchone(config, "SELECT SUM(rows) FROM pg_stat_statements WHERE query ~* '[[:<:]]%s[[:>:]]' AND query ~* 'select'" % cls.tbl_name)

    @classmethod
    def get_rows_updated(cls, config):
        return cls.__exec_and_fetchone(config, "SELECT SUM(rows) FROM pg_stat_statements WHERE query ~* '[[:<:]]%s[[:>:]]' AND query ~* 'update'" % cls.tbl_name)

    @classmethod
    def reset_cache(cls, config):
#        To fix: DISCARD ALL cannot run inside a transaction block
#        cursor = self.db.cursor()
#        cursor.execute("END;DISCARD ALL;")
#        self.db.commit()
        return

    @classmethod
    def __exec_and_fetchone(cls, config, query):
        db = cls.get_connection(config)
        cursor = db.cursor()
        cursor.execute(query)
        record = cursor.fetchone()
        return record[0]
