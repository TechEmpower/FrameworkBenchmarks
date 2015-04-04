import pymysql
import unittest

class PyMySQLTestCase(unittest.TestCase):
    # Edit this to suit your test environment.
    databases = [
        {"host":"localhost","user":"root",
         "passwd":"","db":"test_pymysql", "use_unicode": True},
        {"host":"localhost","user":"root","passwd":"","db":"test_pymysql2"}]

    def setUp(self):
        self.connections = []

        for params in self.databases:
            self.connections.append(pymysql.connect(**params))

    def tearDown(self):
        for connection in self.connections:
            connection.close()

