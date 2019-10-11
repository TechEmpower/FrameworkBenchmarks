import MySQLdb
from toolset.databases.database_check import DatabaseCheck

class MysqlCheck(DatabaseCheck):
    def connect(self):
        self.db = MySQLdb.connect(self.config.database_host,
                                     "benchmarkdbuser", "benchmarkdbpass",
                                     "hello_world")

    def get_queries(self):
        cursor = self.db.cursor()
        cursor.execute("Show session status like 'Queries'")
        record = cursor.fetchone()
        return record[1]

    def get_rows(self):
        cursor = self.db.cursor()
        cursor.execute("show session status like 'Innodb_rows_read'")
        record = cursor.fetchone()
        return record[1]

    def reset_cache(self):
        #No more in Mysql 8.0
        #cursor = self.db.cursor()
        #cursor.execute("RESET QUERY CACHE")
        #self.db.commit()
        return
