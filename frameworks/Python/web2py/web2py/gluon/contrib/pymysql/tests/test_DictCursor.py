from pymysql.tests import base
import pymysql.cursors

import datetime

class TestDictCursor(base.PyMySQLTestCase):

    def test_DictCursor(self):
        #all assert test compare to the structure as would come out from MySQLdb 
        conn = self.connections[0]
        c = conn.cursor(pymysql.cursors.DictCursor)
        # create a table ane some data to query
        c.execute("""CREATE TABLE dictcursor (name char(20), age int , DOB datetime)""")
        data = (("bob",21,"1990-02-06 23:04:56"),
                ("jim",56,"1955-05-09 13:12:45"),
                ("fred",100,"1911-09-12 01:01:01"))
        bob =  {'name':'bob','age':21,'DOB':datetime.datetime(1990, 02, 6, 23, 04, 56)}
        jim =  {'name':'jim','age':56,'DOB':datetime.datetime(1955, 05, 9, 13, 12, 45)}
        fred = {'name':'fred','age':100,'DOB':datetime.datetime(1911, 9, 12, 1, 1, 1)}
        try:
            c.executemany("insert into dictcursor values (%s,%s,%s)", data)
            # try an update which should return no rows
            c.execute("update dictcursor set age=20 where name='bob'")
            bob['age'] = 20
            # pull back the single row dict for bob and check
            c.execute("SELECT * from dictcursor where name='bob'")
            r = c.fetchone()
            self.assertEqual(bob,r,"fetchone via DictCursor failed")
            # same again, but via fetchall => tuple)
            c.execute("SELECT * from dictcursor where name='bob'")
            r = c.fetchall()
            self.assertEqual((bob,),r,"fetch a 1 row result via fetchall failed via DictCursor")
            # same test again but iterate over the 
            c.execute("SELECT * from dictcursor where name='bob'")
            for r in c:
                self.assertEqual(bob, r,"fetch a 1 row result via iteration failed via DictCursor")
            # get all 3 row via fetchall
            c.execute("SELECT * from dictcursor")
            r = c.fetchall()
            self.assertEqual((bob,jim,fred), r, "fetchall failed via DictCursor")
            #same test again but do a list comprehension
            c.execute("SELECT * from dictcursor")
            r = [x for x in c]
            self.assertEqual([bob,jim,fred], r, "list comprehension failed via DictCursor")
            # get all 2 row via fetchmany
            c.execute("SELECT * from dictcursor")
            r = c.fetchmany(2)
            self.assertEqual((bob,jim), r, "fetchmany failed via DictCursor")
        finally:
            c.execute("drop table dictcursor")

__all__ = ["TestDictCursor"]

if __name__ == "__main__":
    import unittest
    unittest.main()
