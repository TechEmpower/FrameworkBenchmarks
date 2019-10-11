import psycopg2
from toolset.databases.database_check import DatabaseCheck

class PostgresCheck(DatabaseCheck):
    def connect(self):
        self.db = psycopg2.connect(
                    host=self.config.database_host,
                    port="5432",
                    user="benchmarkdbuser",
                    password="benchmarkdbpass",
                    database="hello_world")
        cursor=self.db.cursor()
        cursor.execute("CREATE EXTENSION IF NOT EXISTS pg_stat_statements")
        self.db.commit()

    def get_queries(self):
        cursor = self.db.cursor()
        cursor.execute("SELECT SUM(calls) FROM pg_stat_statements WHERE query ~* ' "+self.tbl_name+"'")
        record = cursor.fetchone()
        return record[0]

    def get_rows(self):
        cursor = self.db.cursor()
        cursor.execute("SELECT SUM(rows) FROM pg_stat_statements WHERE query ~* ' "+self.tbl_name+"'")
        record = cursor.fetchone()
        return record[0]

    def reset_cache(self):
#        To fix: DISCARD ALL cannot run inside a transaction block
#        cursor = self.db.cursor()
#        cursor.execute("END;DISCARD ALL;")
#        self.db.commit()
        return
