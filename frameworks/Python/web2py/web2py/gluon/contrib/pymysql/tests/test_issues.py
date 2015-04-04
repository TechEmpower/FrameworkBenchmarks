import pymysql
from pymysql.tests import base
import unittest

import sys

try:
    import imp
    reload = imp.reload
except AttributeError:
    pass

import datetime

# backwards compatibility:
if not hasattr(unittest, "skip"):
    unittest.skip = lambda message: lambda f: f

class TestOldIssues(base.PyMySQLTestCase):
    def test_issue_3(self):
        """ undefined methods datetime_or_None, date_or_None """
        conn = self.connections[0]
        c = conn.cursor()
        c.execute("create table issue3 (d date, t time, dt datetime, ts timestamp)")
        try:
            c.execute("insert into issue3 (d, t, dt, ts) values (%s,%s,%s,%s)", (None, None, None, None))
            c.execute("select d from issue3")
            self.assertEqual(None, c.fetchone()[0])
            c.execute("select t from issue3")
            self.assertEqual(None, c.fetchone()[0])
            c.execute("select dt from issue3")
            self.assertEqual(None, c.fetchone()[0])
            c.execute("select ts from issue3")
            self.assertTrue(isinstance(c.fetchone()[0], datetime.datetime))
        finally:
            c.execute("drop table issue3")

    def test_issue_4(self):
        """ can't retrieve TIMESTAMP fields """
        conn = self.connections[0]
        c = conn.cursor()
        c.execute("create table issue4 (ts timestamp)")
        try:
            c.execute("insert into issue4 (ts) values (now())")
            c.execute("select ts from issue4")
            self.assertTrue(isinstance(c.fetchone()[0], datetime.datetime))
        finally:
            c.execute("drop table issue4")

    def test_issue_5(self):
        """ query on information_schema.tables fails """
        con = self.connections[0]
        cur = con.cursor()
        cur.execute("select * from information_schema.tables")

    def test_issue_6(self):
        """ exception: TypeError: ord() expected a character, but string of length 0 found """
        conn = pymysql.connect(host="localhost",user="root",passwd="",db="mysql")
        c = conn.cursor()
        c.execute("select * from user")
        conn.close()

    def test_issue_8(self):
        """ Primary Key and Index error when selecting data """
        conn = self.connections[0]
        c = conn.cursor()
        c.execute("""CREATE TABLE `test` (`station` int(10) NOT NULL DEFAULT '0', `dh`
datetime NOT NULL DEFAULT '0000-00-00 00:00:00', `echeance` int(1) NOT NULL
DEFAULT '0', `me` double DEFAULT NULL, `mo` double DEFAULT NULL, PRIMARY
KEY (`station`,`dh`,`echeance`)) ENGINE=MyISAM DEFAULT CHARSET=latin1;""")
        try:
            self.assertEqual(0, c.execute("SELECT * FROM test"))
            c.execute("ALTER TABLE `test` ADD INDEX `idx_station` (`station`)")
            self.assertEqual(0, c.execute("SELECT * FROM test"))
        finally:
            c.execute("drop table test")

    def test_issue_9(self):
        """ sets DeprecationWarning in Python 2.6 """
        try:
            reload(pymysql)
        except DeprecationWarning:
            self.fail()

    def test_issue_10(self):
        """ Allocate a variable to return when the exception handler is permissive """
        conn = self.connections[0]
        conn.errorhandler = lambda cursor, errorclass, errorvalue: None
        cur = conn.cursor()
        cur.execute( "create table t( n int )" )
        cur.execute( "create table t( n int )" )

    def test_issue_13(self):
        """ can't handle large result fields """
        conn = self.connections[0]
        cur = conn.cursor()
        try:
            cur.execute("create table issue13 (t text)")
            # ticket says 18k
            size = 18*1024
            cur.execute("insert into issue13 (t) values (%s)", ("x" * size,))
            cur.execute("select t from issue13")
            # use assertTrue so that obscenely huge error messages don't print
            r = cur.fetchone()[0]
            self.assertTrue("x" * size == r)
        finally:
            cur.execute("drop table issue13")

    def test_issue_14(self):
        """ typo in converters.py """
        self.assertEqual('1', pymysql.converters.escape_item(1, "utf8"))
        self.assertEqual('1', pymysql.converters.escape_item(1L, "utf8"))

        self.assertEqual('1', pymysql.converters.escape_object(1))
        self.assertEqual('1', pymysql.converters.escape_object(1L))

    def test_issue_15(self):
        """ query should be expanded before perform character encoding """
        conn = self.connections[0]
        c = conn.cursor()
        c.execute("create table issue15 (t varchar(32))")
        try:
            c.execute("insert into issue15 (t) values (%s)", (u'\xe4\xf6\xfc',))
            c.execute("select t from issue15")
            self.assertEqual(u'\xe4\xf6\xfc', c.fetchone()[0])
        finally:
            c.execute("drop table issue15")

    def test_issue_16(self):
        """ Patch for string and tuple escaping """
        conn = self.connections[0]
        c = conn.cursor()
        c.execute("create table issue16 (name varchar(32) primary key, email varchar(32))")
        try:
            c.execute("insert into issue16 (name, email) values ('pete', 'floydophone')")
            c.execute("select email from issue16 where name=%s", ("pete",))
            self.assertEqual("floydophone", c.fetchone()[0])
        finally:
            c.execute("drop table issue16")

    @unittest.skip("test_issue_17() requires a custom, legacy MySQL configuration and will not be run.")
    def test_issue_17(self):
        """ could not connect mysql use passwod """
        conn = self.connections[0]
        host = self.databases[0]["host"]
        db = self.databases[0]["db"]
        c = conn.cursor()
        # grant access to a table to a user with a password
        try:
            c.execute("create table issue17 (x varchar(32) primary key)")
            c.execute("insert into issue17 (x) values ('hello, world!')")
            c.execute("grant all privileges on %s.issue17 to 'issue17user'@'%%' identified by '1234'" % db)
            conn.commit()
            
            conn2 = pymysql.connect(host=host, user="issue17user", passwd="1234", db=db)
            c2 = conn2.cursor()
            c2.execute("select x from issue17")
            self.assertEqual("hello, world!", c2.fetchone()[0])
        finally:
            c.execute("drop table issue17")

def _uni(s, e):
    # hack for py3
    if sys.version_info[0] > 2:
        return unicode(bytes(s, sys.getdefaultencoding()), e)
    else:
        return unicode(s, e)

class TestNewIssues(base.PyMySQLTestCase):
    def test_issue_34(self):
        try:
            pymysql.connect(host="localhost", port=1237, user="root")
            self.fail()
        except pymysql.OperationalError, e:
            self.assertEqual(2003, e.args[0])
        except:
            self.fail()

    def test_issue_33(self):
        conn = pymysql.connect(host="localhost", user="root", db=self.databases[0]["db"], charset="utf8")
        c = conn.cursor()
        try:
            c.execute(_uni("create table hei\xc3\x9fe (name varchar(32))", "utf8"))
            c.execute(_uni("insert into hei\xc3\x9fe (name) values ('Pi\xc3\xb1ata')", "utf8"))
            c.execute(_uni("select name from hei\xc3\x9fe", "utf8"))
            self.assertEqual(_uni("Pi\xc3\xb1ata","utf8"), c.fetchone()[0])
        finally:
            c.execute(_uni("drop table hei\xc3\x9fe", "utf8"))

    @unittest.skip("This test requires manual intervention")
    def test_issue_35(self):
        conn = self.connections[0]
        c = conn.cursor()
        print "sudo killall -9 mysqld within the next 10 seconds"
        try:
            c.execute("select sleep(10)")
            self.fail()
        except pymysql.OperationalError, e:
            self.assertEqual(2013, e.args[0])

    def test_issue_36(self):
        conn = self.connections[0]
        c = conn.cursor()
        # kill connections[0]
        c.execute("show processlist")
        kill_id = None
        for id,user,host,db,command,time,state,info in c.fetchall():
            if info == "show processlist":
                kill_id = id
                break
        # now nuke the connection
        conn.kill(kill_id)
        # make sure this connection has broken
        try:
            c.execute("show tables")
            self.fail()
        except:
            pass
        # check the process list from the other connection
        try:
            c = self.connections[1].cursor()
            c.execute("show processlist")
            ids = [row[0] for row in c.fetchall()]
            self.assertFalse(kill_id in ids)
        finally:
            del self.connections[0]

    def test_issue_37(self):
        conn = self.connections[0]
        c = conn.cursor()
        self.assertEqual(1, c.execute("SELECT @foo"))
        self.assertEqual((None,), c.fetchone())
        self.assertEqual(0, c.execute("SET @foo = 'bar'"))
        c.execute("set @foo = 'bar'")

    def test_issue_38(self):
        conn = self.connections[0]
        c = conn.cursor()
        datum = "a" * 1024 * 1023 # reduced size for most default mysql installs
        
        try:
            c.execute("create table issue38 (id integer, data mediumblob)")
            c.execute("insert into issue38 values (1, %s)", (datum,))
        finally:
            c.execute("drop table issue38")

    def disabled_test_issue_54(self):
        conn = self.connections[0]
        c = conn.cursor()
        big_sql = "select * from issue54 where "
        big_sql += " and ".join("%d=%d" % (i,i) for i in xrange(0, 100000))

        try:
            c.execute("create table issue54 (id integer primary key)")
            c.execute("insert into issue54 (id) values (7)")
            c.execute(big_sql)
            self.assertEqual(7, c.fetchone()[0])
        finally:
            c.execute("drop table issue54")

class TestGitHubIssues(base.PyMySQLTestCase):
    def test_issue_66(self):
        conn = self.connections[0]
        c = conn.cursor()
        self.assertEqual(0, conn.insert_id())
        try:
            c.execute("create table issue66 (id integer primary key auto_increment, x integer)")
            c.execute("insert into issue66 (x) values (1)")
            c.execute("insert into issue66 (x) values (1)")
            self.assertEqual(2, conn.insert_id())
        finally:
            c.execute("drop table issue66")

__all__ = ["TestOldIssues", "TestNewIssues", "TestGitHubIssues"]

if __name__ == "__main__":
    import unittest
    unittest.main()
