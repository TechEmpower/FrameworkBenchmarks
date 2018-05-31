import MySQLdb
import psycopg2
import pymongo
import pyodbc


def test_database(config, database_name):
    if database_name == "mysql":
        try:
            db = MySQLdb.connect(config.database_host, "benchmarkdbuser",
                                 "benchmarkdbpass", "hello_world")
            cursor = db.cursor()
            cursor.execute("SELECT 1")
            cursor.fetchall()
            db.close()
        except:
            return False
    elif database_name == "postgres":
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
        except:
            return False
    elif database_name == "mongodb":
        try:
            connection = pymongo.MongoClient(host=config.database_host)
            db = connection.hello_world
            db.world.find()
            db.close()
        except:
            return False
    elif database_name == "sqlserver":
        try:
            db = pyodbc.connect(
                "Driver={{ODBC Driver 17 for SQL Server}};"
                "Server={0};"
                "Port=1433;"
                "Database=hello_world;"
                "UID=benchmarkdbuser;"
                "PWD=benchmarkdbpass;".format(config.database_host))
            cursor = db.cursor()
            cursor.execute("SELECT 1")
            cursor.fetchall()
            db.close()
        except:
            return False

    return True