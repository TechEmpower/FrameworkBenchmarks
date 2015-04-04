# -*- coding: utf-8 -*-
"""
    Unit tests for NoSQL adapters
"""

import sys
import os
import glob
import datetime
try:
    import cStringIO as StringIO
except:
    from io import StringIO
from ._compat import unittest


from pydal import DAL, Field
from pydal.objects import Table
from pydal.helpers.classes import SQLALL
from ._adapt import DEFAULT_URI, IS_IMAP, drop, IS_GAE, IS_MONGODB

if IS_IMAP:
    from pydal.adapters import IMAPAdapter
    from pydal.contrib import mockimaplib
    IMAPAdapter.driver = mockimaplib
elif IS_GAE:
    # setup GAE dummy database
    from google.appengine.ext import testbed
    gaetestbed = testbed.Testbed()
    gaetestbed.activate()
    gaetestbed.init_datastore_v3_stub()
    gaetestbed.init_memcache_stub()

print 'Testing against %s engine (%s)' % (DEFAULT_URI.partition(':')[0],
                                          DEFAULT_URI)


ALLOWED_DATATYPES = [
    'string',
    'text',
    'integer',
    'boolean',
    'double',
    'blob',
    'date',
    'time',
    'datetime',
    'upload',
    'password',
    'json',
    ]


def setUpModule():
    pass

def tearDownModule():
    if os.path.isfile('sql.log'):
        os.unlink('sql.log')
    for a in glob.glob('*.table'):
        os.unlink(a)

@unittest.skipIf(IS_IMAP, "Skip IMAP")
class TestFields(unittest.TestCase):

    def testFieldName(self):

        # Check that Fields cannot start with underscores
        self.assertRaises(SyntaxError, Field, '_abc', 'string')

        # Check that Fields cannot contain punctuation other than underscores
        self.assertRaises(SyntaxError, Field, 'a.bc', 'string')

        # Check that Fields cannot be a name of a method or property of Table
        for x in ['drop', 'on', 'truncate']:
            self.assertRaises(SyntaxError, Field, x, 'string')

        # Check that Fields allows underscores in the body of a field name.
        self.assert_(Field('a_bc', 'string'),
            "Field isn't allowing underscores in fieldnames.  It should.")

    def testFieldTypes(self):

        # Check that string, and password default length is 512
        for typ in ['string', 'password']:
            self.assert_(Field('abc', typ).length == 512,
                         "Default length for type '%s' is not 512 or 255" % typ)

        # Check that upload default length is 512
        self.assert_(Field('abc', 'upload').length == 512,
                     "Default length for type 'upload' is not 512")

        # Check that Tables passed in the type creates a reference
        self.assert_(Field('abc', Table(None, 'temp')).type
                      == 'reference temp',
                     'Passing an Table does not result in a reference type.')

    def testFieldLabels(self):

        # Check that a label is successfully built from the supplied fieldname
        self.assert_(Field('abc', 'string').label == 'Abc',
                     'Label built is incorrect')
        self.assert_(Field('abc_def', 'string').label == 'Abc Def',
                     'Label built is incorrect')

    def testFieldFormatters(self):  # Formatter should be called Validator

        # Test the default formatters
        for typ in ALLOWED_DATATYPES:
            f = Field('abc', typ)
            if typ not in ['date', 'time', 'datetime']:
                isinstance(f.formatter('test'), str)
            else:
                isinstance(f.formatter(datetime.datetime.now()), str)

    @unittest.skipIf(IS_GAE or IS_MONGODB, 'TODO: Datastore does accept dict objects as json field input. MongoDB assertion error Binary("x", 0) != "x"')
    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        for ft in ['string', 'text', 'password', 'upload', 'blob']:
            db.define_table('tt', Field('aa', ft, default=''))
            self.assertEqual(isinstance(db.tt.insert(aa='x'), long), True)
            self.assertEqual(db().select(db.tt.aa)[0].aa, 'x')
            drop(db.tt)
        db.define_table('tt', Field('aa', 'integer', default=1))
        self.assertEqual(isinstance(db.tt.insert(aa=3), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, 3)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'double', default=1))
        self.assertEqual(isinstance(db.tt.insert(aa=3.1), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, 3.1)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'boolean', default=True))
        self.assertEqual(isinstance(db.tt.insert(aa=True), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, True)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'json', default={}))
        self.assertEqual(isinstance(db.tt.insert(aa={}), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, {})
        drop(db.tt)
        db.define_table('tt', Field('aa', 'date',
                        default=datetime.date.today()))
        t0 = datetime.date.today()
        self.assertEqual(isinstance(db.tt.insert(aa=t0), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, t0)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'datetime',
                        default=datetime.datetime.today()))
        t0 = datetime.datetime(
            1971,
            12,
            21,
            10,
            30,
            55,
            0,
            )
        self.assertEqual(isinstance(db.tt.insert(aa=t0), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, t0)

        ## Row APIs
        row = db().select(db.tt.aa)[0]
        self.assertEqual(db.tt[1].aa,t0)
        self.assertEqual(db.tt['aa'],db.tt.aa)
        self.assertEqual(db.tt(1).aa,t0)
        self.assertTrue(db.tt(1,aa=None)==None)
        self.assertFalse(db.tt(1,aa=t0)==None)
        self.assertEqual(row.aa,t0)
        self.assertEqual(row['aa'],t0)
        self.assertEqual(row['tt.aa'],t0)
        self.assertEqual(row('tt.aa'),t0)

        ## Lazy and Virtual fields
        db.tt.b = Field.Virtual(lambda row: row.tt.aa)
        db.tt.c = Field.Lazy(lambda row: row.tt.aa)
        row = db().select(db.tt.aa)[0]
        self.assertEqual(row.b,t0)
        self.assertEqual(row.c(),t0)

        drop(db.tt)
        db.define_table('tt', Field('aa', 'time', default='11:30'))
        t0 = datetime.time(10, 30, 55)
        self.assertEqual(isinstance(db.tt.insert(aa=t0), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, t0)
        drop(db.tt)


@unittest.skipIf(IS_IMAP, "Skip IMAP")
class TestTables(unittest.TestCase):

    def testTableNames(self):

        # Check that Tables cannot start with underscores
        self.assertRaises(SyntaxError, Table, None, '_abc')

        # Check that Tables cannot contain punctuation other than underscores
        self.assertRaises(SyntaxError, Table, None, 'a.bc')

        # Check that Tables cannot be a name of a method or property of DAL
        for x in ['define_table', 'tables', 'as_dict']:
            self.assertRaises(SyntaxError, Table, None, x)

        # Check that Table allows underscores in the body of a field name.
        self.assert_(Table(None, 'a_bc'),
            "Table isn't allowing underscores in tablename.  It should.")

@unittest.skipIf(IS_IMAP, "Skip IMAP")
class TestAll(unittest.TestCase):

    def setUp(self):
        self.pt = Table(None,'PseudoTable',Field('name'),Field('birthdate'))

    def testSQLALL(self):
        ans = 'PseudoTable.id, PseudoTable.name, PseudoTable.birthdate'
        self.assertEqual(str(SQLALL(self.pt)), ans)

@unittest.skipIf(IS_IMAP, "Skip IMAP")
class TestTable(unittest.TestCase):

    def testTableCreation(self):

        # Check for error when not passing type other than Field or Table

        self.assertRaises(SyntaxError, Table, None, 'test', None)

        persons = Table(None, 'persons',
                        Field('firstname','string'),
                        Field('lastname', 'string'))

        # Does it have the correct fields?

        self.assert_(set(persons.fields).issuperset(set(['firstname',
                                                         'lastname'])))

        # ALL is set correctly

        self.assert_('persons.firstname, persons.lastname'
                      in str(persons.ALL))

    def testTableAlias(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        persons = Table(db, 'persons', Field('firstname',
                           'string'), Field('lastname', 'string'))
        aliens = persons.with_alias('aliens')

        # Are the different table instances with the same fields

        self.assert_(persons is not aliens)
        self.assert_(set(persons.fields) == set(aliens.fields))

    def testTableInheritance(self):
        persons = Table(None, 'persons', Field('firstname',
                           'string'), Field('lastname', 'string'))
        customers = Table(None, 'customers',
                             Field('items_purchased', 'integer'),
                             persons)
        self.assert_(set(customers.fields).issuperset(set(
            ['items_purchased', 'firstname', 'lastname'])))


class TestInsert(unittest.TestCase):
    def testRun(self):
        if IS_IMAP:
            imap = DAL(DEFAULT_URI)
            imap.define_tables()
            self.assertEqual(imap.Draft.insert(to="nurse@example.com",
                                               subject="Nurse!",
                                               sender="gumby@example.com",
                                               content="Nurse!\r\nNurse!"), 2)
            self.assertEqual(imap.Draft[2].subject, "Nurse!")
            self.assertEqual(imap.Draft[2].sender, "gumby@example.com")
            self.assertEqual(isinstance(imap.Draft[2].uid, long), True)
            self.assertEqual(imap.Draft[2].content[0]["text"], "Nurse!\r\nNurse!")
        else:
            db = DAL(DEFAULT_URI, check_reserved=['all'])
            db.define_table('tt', Field('aa'))
            self.assertEqual(isinstance(db.tt.insert(aa='1'), long), True)
            self.assertEqual(isinstance(db.tt.insert(aa='1'), long), True)
            self.assertEqual(isinstance(db.tt.insert(aa='1'), long), True)
            self.assertEqual(db(db.tt.aa == '1').count(), 3)
            self.assertEqual(db(db.tt.aa == '2').isempty(), True)
            self.assertEqual(db(db.tt.aa == '1').update(aa='2'), 3)
            self.assertEqual(db(db.tt.aa == '2').count(), 3)
            self.assertEqual(db(db.tt.aa == '2').isempty(), False)
            self.assertEqual(db(db.tt.aa == '2').delete(), 3)
            self.assertEqual(db(db.tt.aa == '2').isempty(), True)
            drop(db.tt)


@unittest.skipIf(IS_IMAP, "Skip IMAP")
class TestSelect(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'))
        self.assertEqual(isinstance(db.tt.insert(aa='1'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa='2'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa='3'), long), True)
        self.assertEqual(db(db.tt.id > 0).count(), 3)
        self.assertEqual(db(db.tt.aa).count(), 3)
        self.assertEqual(db(db.tt.id).count(), 3)
        self.assertEqual(db(db.tt.id!=None).count(), 3)

        self.assertEqual(db(db.tt.id > 0).select(orderby=~db.tt.aa | db.tt.id)[0].aa, '3')
        self.assertEqual(db(db.tt.id > 0).select(orderby=~db.tt.aa)[0].aa, '3')
        self.assertEqual(len(db(db.tt.id > 0).select(limitby=(1, 2))), 1)
        self.assertEqual(db(db.tt.id > 0).select(limitby=(1, 2))[0].aa,
                         '2')
        self.assertEqual(len(db().select(db.tt.ALL)), 3)
        self.assertEqual(db(db.tt.aa == None).count(), 0)
        self.assertEqual(db(db.tt.aa != None).count(), 3)
        self.assertEqual(db(db.tt.aa > '1').count(), 2)
        self.assertEqual(db(db.tt.aa >= '1').count(), 3)
        self.assertEqual(db(db.tt.aa == '1').count(), 1)
        self.assertEqual(db(db.tt.aa != '1').count(), 2)
        self.assertEqual(db(db.tt.aa < '3').count(), 2)
        self.assertEqual(db(db.tt.aa <= '3').count(), 3)
        self.assertEqual(db(db.tt.aa > '1')(db.tt.aa < '3').count(), 1)
        self.assertEqual(db((db.tt.aa > '1') & (db.tt.aa < '3')).count(), 1)
        self.assertEqual(db((db.tt.aa > '1') | (db.tt.aa < '3')).count(), 3)
        # Test not operator
        self.assertEqual(db((db.tt.aa > '1') & ~(db.tt.aa > '2')).count(), 1)
        self.assertEqual(db(~(db.tt.aa > '1') & (db.tt.aa > '2')).count(), 0)
        self.assertEqual(db(~((db.tt.aa < '1') | (db.tt.aa > '2'))).count(), 2)
        self.assertEqual(db(~((db.tt.aa >= '1') & (db.tt.aa <= '2'))).count(), 1)
        drop(db.tt)

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestAddMethod(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'))
        @db.tt.add_method.all
        def select_all(table,orderby=None):
            return table._db(table).select(orderby=orderby)
        self.assertEqual(isinstance(db.tt.insert(aa='1'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa='2'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa='3'), long), True)
        self.assertEqual(len(db.tt.all()), 3)
        drop(db.tt)

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestBelongs(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'))
        i_id = db.tt.insert(aa='1')
        self.assertEqual(isinstance(i_id, long), True)
        self.assertEqual(isinstance(db.tt.insert(aa='2'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa='3'), long), True)
        self.assertEqual(db(db.tt.aa.belongs(('1', '3'))).count(), 2)
        self.assertEqual(db(db.tt.aa.belongs(['1', '3'])).count(), 2)
        self.assertEqual(db(db.tt.aa.belongs(['1', '3'])).count(), 2)
        self.assertEqual(db(db.tt.id.belongs([i_id])).count(), 1)

        if not (IS_GAE or IS_MONGODB):
            self.assertEqual(db(db.tt.aa.belongs(db(db.tt.id > 2)._select(db.tt.aa))).count(), 1)

            self.assertEqual(db(db.tt.aa.belongs(db(db.tt.aa.belongs(('1',
                         '3')))._select(db.tt.aa))).count(), 2)
            self.assertEqual(db(db.tt.aa.belongs(db(db.tt.aa.belongs(db
                         (db.tt.aa.belongs(('1', '3')))._select(db.tt.aa)))._select(
                         db.tt.aa))).count(),
                         2)
        else:
            print "Datastore/Mongodb belongs does not accept queries (skipping)"
        drop(db.tt)


@unittest.skipIf(IS_GAE or IS_IMAP, "Contains not supported on GAE Datastore. TODO: IMAP tests")
class TestContains(unittest.TestCase):
    @unittest.skipIf(IS_MONGODB, "TODO: MongoDB Contains error")
    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa', 'list:string'), Field('bb','string'))
        self.assertEqual(isinstance(db.tt.insert(aa=['aaa','bbb'],bb='aaa'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa=['bbb','ddd'],bb='abb'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa=['eee','aaa'],bb='acc'), long), True)
        self.assertEqual(db(db.tt.aa.contains('aaa')).count(), 2)
        self.assertEqual(db(db.tt.aa.contains('bbb')).count(), 2)
        self.assertEqual(db(db.tt.aa.contains('aa')).count(), 0)
        self.assertEqual(db(db.tt.bb.contains('a')).count(), 3)
        self.assertEqual(db(db.tt.bb.contains('b')).count(), 1)
        self.assertEqual(db(db.tt.bb.contains('d')).count(), 0)
        self.assertEqual(db(db.tt.aa.contains(db.tt.bb)).count(), 1)
        drop(db.tt)


@unittest.skipIf(IS_GAE or IS_MONGODB or IS_IMAP, "Like not supported on GAE Datastore. TODO: IMAP test")
class TestLike(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'))
        self.assertEqual(isinstance(db.tt.insert(aa='abc'), long), True)
        self.assertEqual(db(db.tt.aa.like('a%')).count(), 1)
        self.assertEqual(db(db.tt.aa.like('%b%')).count(), 1)
        self.assertEqual(db(db.tt.aa.like('%c')).count(), 1)
        self.assertEqual(db(db.tt.aa.like('%d%')).count(), 0)
        self.assertEqual(db(db.tt.aa.lower().like('A%')).count(), 1)
        self.assertEqual(db(db.tt.aa.lower().like('%B%')).count(),
                         1)
        self.assertEqual(db(db.tt.aa.lower().like('%C')).count(), 1)
        self.assertEqual(db(db.tt.aa.upper().like('A%')).count(), 1)
        self.assertEqual(db(db.tt.aa.upper().like('%B%')).count(),
                         1)
        self.assertEqual(db(db.tt.aa.upper().like('%C')).count(), 1)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'integer'))
        self.assertEqual(isinstance(db.tt.insert(aa=1111111111), long), True)
        self.assertEqual(db(db.tt.aa.like('1%')).count(), 1)
        self.assertEqual(db(db.tt.aa.like('2%')).count(), 0)
        drop(db.tt)

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestDatetime(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa', 'datetime'))
        self.assertEqual(isinstance(db.tt.insert(aa=datetime.datetime(1971, 12, 21,
                         11, 30)), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa=datetime.datetime(1971, 11, 21,
                         10, 30)), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa=datetime.datetime(1970, 12, 21,
                         9, 30)), long), True)
        self.assertEqual(db(db.tt.aa == datetime.datetime(1971, 12,
                         21, 11, 30)).count(), 1)
        self.assertEqual(db(db.tt.aa >= datetime.datetime(1971, 1, 1)).count(), 2)
        drop(db.tt)

@unittest.skipIf(IS_GAE or IS_MONGODB or IS_IMAP, "Expressions are not supported")
class TestExpressions(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa', 'integer'))
        self.assertEqual(isinstance(db.tt.insert(aa=1), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa=2), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa=3), long), True)
        self.assertEqual(db(db.tt.aa == 3).update(aa=db.tt.aa + 1), 1)
        self.assertEqual(db(db.tt.aa == 4).count(), 1)
        self.assertEqual(db(db.tt.aa == -2).count(), 0)
        sum = (db.tt.aa + 1).sum()
        self.assertEqual(db(db.tt.aa == 2).select(sum).first()[sum], 3)
        self.assertEqual(db(db.tt.aa == -2).select(sum).first()[sum], None)
        drop(db.tt)


@unittest.skip("JOIN queries are not supported")
class TestJoin(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('t1', Field('aa'))
        db.define_table('t2', Field('aa'), Field('b', db.t1))
        i1 = db.t1.insert(aa='1')
        i2 = db.t1.insert(aa='2')
        i3 = db.t1.insert(aa='3')
        db.t2.insert(aa='4', b=i1)
        db.t2.insert(aa='5', b=i2)
        db.t2.insert(aa='6', b=i2)
        self.assertEqual(len(db(db.t1.id
                          == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)), 3)
        self.assertEqual(db(db.t1.id == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)[2].t1.aa, '2')
        self.assertEqual(db(db.t1.id == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)[2].t2.aa, '6')
        self.assertEqual(len(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)), 4)
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[2].t1.aa, '2')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[2].t2.aa, '6')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[3].t1.aa, '3')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[3].t2.aa, None)
        self.assertEqual(len(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa, groupby=db.t1.aa)),
                         3)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[0]._extra[db.t2.id.count()],
                         1)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[1]._extra[db.t2.id.count()],
                         2)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[2]._extra[db.t2.id.count()],
                         0)
        drop(db.t2)
        drop(db.t1)

        db.define_table('person',Field('name'))
        id = db.person.insert(name="max")
        self.assertEqual(id.name,'max')
        db.define_table('dog',Field('name'),Field('ownerperson','reference person'))
        db.dog.insert(name='skipper',ownerperson=1)
        row = db(db.person.id==db.dog.ownerperson).select().first()
        self.assertEqual(row[db.person.name],'max')
        self.assertEqual(row['person.name'],'max')
        drop(db.dog)
        self.assertEqual(len(db.person._referenced_by),0)
        drop(db.person)

class TestMinMaxSumAvg(unittest.TestCase):
    @unittest.skipIf(IS_GAE or IS_MONGODB or IS_IMAP, 'TODO: Datastore throws "AttributeError: Row object has no attribute _extra"')
    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa', 'integer'))
        self.assertEqual(isinstance(db.tt.insert(aa=1), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa=2), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa=3), long), True)
        s = db.tt.aa.min()
        self.assertEqual(db(db.tt.id > 0).select(s)[0]._extra[s], 1)
        self.assertEqual(db(db.tt.id > 0).select(s).first()[s], 1)
        self.assertEqual(db().select(s).first()[s], 1)
        s = db.tt.aa.max()
        self.assertEqual(db().select(s).first()[s], 3)
        s = db.tt.aa.sum()
        self.assertEqual(db().select(s).first()[s], 6)
        s = db.tt.aa.count()
        self.assertEqual(db().select(s).first()[s], 3)
        s = db.tt.aa.avg()
        self.assertEqual(db().select(s).first()[s], 2)
        drop(db.tt)


@unittest.skipIf(IS_IMAP, "Skip IMAP")
class TestMigrations(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'), migrate='.storage.table')
        db.commit()
        db.close()
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'), Field('b'),
                        migrate='.storage.table')
        db.commit()
        db.close()
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'), Field('b', 'text'),
                        migrate='.storage.table')
        db.commit()
        db.close()
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'), migrate='.storage.table')
        drop(db.tt)
        db.commit()
        db.close()

    def tearDown(self):
        if os.path.exists('.storage.db'):
            os.unlink('.storage.db')
        if os.path.exists('.storage.table'):
            os.unlink('.storage.table')

class TestReference(unittest.TestCase):
    @unittest.skipIf(IS_MONGODB or IS_IMAP, "TODO: MongoDB assertion error (long object has no attribute id)")
    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        if DEFAULT_URI.startswith('mssql'):
            #multiple cascade gotcha
            for key in ['reference','reference FK']:
                db._adapter.types[key]=db._adapter.types[key].replace(
                '%(on_delete_action)s','NO ACTION')
        db.define_table('tt', Field('name'), Field('aa','reference tt'))
        db.commit()
        x = db.tt.insert(name='max')
        assert isinstance(x.id, long) == True
        assert isinstance(x['id'], long) == True
        x.aa = x
        assert isinstance(x.aa, long) == True
        x.update_record()
        y = db.tt[x.id]
        assert y.aa == x.aa
        assert y.aa.aa.aa.aa.aa.aa.name == 'max'
        z=db.tt.insert(name='xxx', aa = y)
        assert z.aa == y.id
        drop(db.tt)
        db.commit()

@unittest.skipIf(IS_IMAP, "Skip IMAP")
class TestClientLevelOps(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'))
        db.commit()
        db.tt.insert(aa="test")
        rows1 = db(db.tt.aa=='test').select()
        rows2 = db(db.tt.aa=='test').select()
        rows3 = rows1 & rows2
        assert len(rows3) == 2
        rows4 = rows1 | rows2
        assert len(rows4) == 1
        rows5 = rows1.find(lambda row: row.aa=="test")
        assert len(rows5) == 1
        rows6 = rows2.exclude(lambda row: row.aa=="test")
        assert len(rows6) == 1
        rows7 = rows5.sort(lambda row: row.aa)
        assert len(rows7) == 1
        drop(db.tt)
        db.commit()

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestVirtualFields(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'))
        db.commit()
        db.tt.insert(aa="test")
        class Compute:
            def a_upper(row): return row.tt.aa.upper()
        db.tt.virtualfields.append(Compute())
        assert db(db.tt.id>0).select().first().a_upper == 'TEST'
        drop(db.tt)
        db.commit()

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestComputedFields(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt',
                        Field('aa'),
                        Field('bb',default='x'),
                        Field('cc',compute=lambda r: r.aa+r.bb))
        db.commit()
        id = db.tt.insert(aa="z")
        self.assertEqual(db.tt[id].cc,'zx')
        drop(db.tt)
        db.commit()

        # test checking that a compute field can refer to earlier-defined computed fields
        db.define_table('tt',
                        Field('aa'),
                        Field('bb',default='x'),
                        Field('cc',compute=lambda r: r.aa+r.bb),
                        Field('dd',compute=lambda r: r.bb + r.cc))
        db.commit()
        id = db.tt.insert(aa="z")
        self.assertEqual(db.tt[id].dd,'xzx')
        drop(db.tt)
        db.commit()

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestCommonFilters(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('t1', Field('aa', 'integer'))
        db.define_table('t2', Field('aa', 'integer'), Field('b', db.t1))
        i1 = db.t1.insert(aa=1)
        i2 = db.t1.insert(aa=2)
        i3 = db.t1.insert(aa=3)
        db.t2.insert(aa=4, b=i1)
        db.t2.insert(aa=5, b=i2)
        db.t2.insert(aa=6, b=i2)
        db.t1._common_filter = lambda q: db.t1.aa>1
        self.assertEqual(db(db.t1).count(),2)
        self.assertEqual(db(db.t1).count(),2)
        db.t2._common_filter = lambda q: db.t2.aa<6
        # test delete
        self.assertEqual(db(db.t2).count(),2)
        db(db.t2).delete()
        self.assertEqual(db(db.t2).count(),0)
        db.t2._common_filter = None
        self.assertEqual(db(db.t2).count(),1)
        # test update
        db.t2.insert(aa=4, b=i1)
        db.t2.insert(aa=5, b=i2)
        db.t2._common_filter = lambda q: db.t2.aa<6
        self.assertEqual(db(db.t2).count(),2)
        db(db.t2).update(aa=6)
        self.assertEqual(db(db.t2).count(),0)
        db.t2._common_filter = None
        self.assertEqual(db(db.t2).count(),3)
        drop(db.t2)
        drop(db.t1)

@unittest.skipIf(IS_IMAP, "Skip IMAP test")
class TestImportExportFields(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('person', Field('name'))
        db.define_table('pet',Field('friend',db.person),Field('name'))
        for n in range(2):
            db(db.pet).delete()
            db(db.person).delete()
            for k in range(10):
                id = db.person.insert(name=str(k))
                db.pet.insert(friend=id,name=str(k))
        db.commit()
        stream = StringIO.StringIO()
        db.export_to_csv_file(stream)
        db(db.pet).delete()
        db(db.person).delete()
        stream = StringIO.StringIO(stream.getvalue())
        db.import_from_csv_file(stream)
        assert db(db.person).count()==10
        assert db(db.pet.name).count()==10
        drop(db.pet)
        drop(db.person)
        db.commit()

@unittest.skipIf(IS_IMAP, "Skip IMAP test")
class TestImportExportUuidFields(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('person', Field('name'),Field('uuid'))
        db.define_table('pet',Field('friend',db.person),Field('name'))
        for n in range(2):
            db(db.pet).delete()
            db(db.person).delete()
            for k in range(10):
                id = db.person.insert(name=str(k),uuid=str(k))
                db.pet.insert(friend=id,name=str(k))
        db.commit()
        stream = StringIO.StringIO()
        db.export_to_csv_file(stream)
        db(db.person).delete()
        db(db.pet).delete()
        stream = StringIO.StringIO(stream.getvalue())
        db.import_from_csv_file(stream)
        assert db(db.person).count()==10
        assert db(db.pet).count()==10
        drop(db.pet)
        drop(db.person)
        db.commit()

@unittest.skipIf(IS_IMAP, "Skip IMAP test")
class TestDALDictImportExport(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('person', Field('name', default="Michael"),Field('uuid'))
        db.define_table('pet',Field('friend',db.person),Field('name'))
        dbdict = db.as_dict(flat=True, sanitize=False)
        assert isinstance(dbdict, dict)
        uri = dbdict["uri"]
        assert isinstance(uri, basestring) and uri
        assert len(dbdict["tables"]) == 2
        assert len(dbdict["tables"][0]["fields"]) == 3
        assert dbdict["tables"][0]["fields"][1]["type"] == db.person.name.type
        assert dbdict["tables"][0]["fields"][1]["default"] == db.person.name.default

        db2 = DAL(**dbdict)
        assert len(db.tables) == len(db2.tables)
        assert hasattr(db2, "pet") and isinstance(db2.pet, Table)
        assert hasattr(db2.pet, "friend") and isinstance(db2.pet.friend, Field)
        drop(db.pet)
        db.commit()

        db2.commit()

        have_serializers = True
        try:
            import serializers
            dbjson = db.as_json(sanitize=False)
            assert isinstance(dbjson, basestring) and len(dbjson) > 0

            unicode_keys = True
            if sys.version < "2.6.5":
                unicode_keys = False
            db3 = DAL(**serializers.loads_json(dbjson,
                          unicode_keys=unicode_keys))
            assert hasattr(db3, "person") and hasattr(db3.person, "uuid") and\
            db3.person.uuid.type == db.person.uuid.type
            drop(db3.person)
            db3.commit()
        except ImportError:
            pass

        mpfc = "Monty Python's Flying Circus"
        dbdict4 = {"uri": DEFAULT_URI,
                   "tables":[{"tablename": "tvshow",
                              "fields": [{"fieldname": "name",
                                          "default":mpfc},
                                         {"fieldname": "rating",
                                          "type":"double"}]},
                             {"tablename": "staff",
                              "fields": [{"fieldname": "name",
                                          "default":"Michael"},
                                         {"fieldname": "food",
                                          "default":"Spam"},
                                         {"fieldname": "tvshow",
                                          "type": "reference tvshow"}]}]}
        db4 = DAL(**dbdict4)
        assert "staff" in db4.tables
        assert "name" in db4.staff
        assert db4.tvshow.rating.type == "double"
        assert (isinstance(db4.tvshow.insert(), long), isinstance(db4.tvshow.insert(name="Loriot"), long),
                isinstance(db4.tvshow.insert(name="Il Mattatore"), long)) == (True, True, True)
        assert isinstance(db4(db4.tvshow).select().first().id, long) == True
        assert db4(db4.tvshow).select().first().name == mpfc

        drop(db4.staff)
        drop(db4.tvshow)
        db4.commit()

        dbdict5 = {"uri": DEFAULT_URI}
        db5 = DAL(**dbdict5)
        assert db5.tables in ([], None)
        assert not (str(db5) in ("", None))

        dbdict6 = {"uri": DEFAULT_URI,
                   "tables":[{"tablename": "staff"},
                             {"tablename": "tvshow",
                              "fields": [{"fieldname": "name"},
                                         {"fieldname": "rating", "type":"double"}
                                        ]
                             }]
                  }
        db6 = DAL(**dbdict6)

        assert len(db6["staff"].fields) == 1
        assert "name" in db6["tvshow"].fields

        assert db6.staff.insert() is not None
        assert isinstance(db6(db6.staff).select().first().id, long) == True


        drop(db6.staff)
        drop(db6.tvshow)
        db6.commit()


@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestSelectAsDict(unittest.TestCase):

    def testSelect(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table(
            'a_table',
            Field('b_field'),
            Field('a_field'),
            )
        db.a_table.insert(a_field="aa1", b_field="bb1")
        rtn = db(db.a_table).select(db.a_table.id, db.a_table.b_field, db.a_table.a_field).as_list()
        self.assertEqual(rtn[0]['b_field'], 'bb1')
        keys = rtn[0].keys()
        self.assertEqual(len(keys), 3)
        self.assertEqual(("id" in keys, "b_field" in keys, "a_field" in keys), (True, True, True))
        drop(db.a_table)


@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestRNameTable(unittest.TestCase):
    #tests for highly experimental rname attribute
    @unittest.skipIf(IS_MONGODB, "TODO: MongoDB assertion error (long object has no attribute id)")
    def testSelect(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        rname = db._adapter.QUOTE_TEMPLATE % 'a very complicated tablename'
        db.define_table(
            'easy_name',
            Field('a_field'),
            rname=rname
            )
        rtn = db.easy_name.insert(a_field='a')
        self.assertEqual(isinstance(rtn.id, long), True)
        rtn = db(db.easy_name.a_field == 'a').select()
        self.assertEqual(len(rtn), 1)
        self.assertEqual(isinstance(rtn[0].id, long), True)
        self.assertEqual(rtn[0].a_field, 'a')
        db.easy_name.insert(a_field='b')
        self.assertEqual(db(db.easy_name).count(), 2)
        rtn = db(db.easy_name.a_field == 'a').update(a_field='c')
        self.assertEqual(rtn, 1)

        #clean up
        drop(db.easy_name)

    @unittest.skip("JOIN queries are not supported")
    def testJoin(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        rname = db._adapter.QUOTE_TEMPLATE % 'this is table t1'
        rname2 = db._adapter.QUOTE_TEMPLATE % 'this is table t2'
        db.define_table('t1', Field('aa'), rname=rname)
        db.define_table('t2', Field('aa'), Field('b', db.t1), rname=rname2)
        i1 = db.t1.insert(aa='1')
        i2 = db.t1.insert(aa='2')
        i3 = db.t1.insert(aa='3')
        db.t2.insert(aa='4', b=i1)
        db.t2.insert(aa='5', b=i2)
        db.t2.insert(aa='6', b=i2)
        self.assertEqual(len(db(db.t1.id
                          == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)), 3)
        self.assertEqual(db(db.t1.id == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)[2].t1.aa, '2')
        self.assertEqual(db(db.t1.id == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)[2].t2.aa, '6')
        self.assertEqual(len(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)), 4)
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[2].t1.aa, '2')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[2].t2.aa, '6')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[3].t1.aa, '3')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[3].t2.aa, None)
        self.assertEqual(len(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa, groupby=db.t1.aa)),
                         3)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[0]._extra[db.t2.id.count()],
                         1)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[1]._extra[db.t2.id.count()],
                         2)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[2]._extra[db.t2.id.count()],
                         0)
        drop(db.t2)
        drop(db.t1)

        db.define_table('person',Field('name'), rname=rname)
        id = db.person.insert(name="max")
        self.assertEqual(id.name,'max')
        db.define_table('dog',Field('name'),Field('ownerperson','reference person'), rname=rname2)
        db.dog.insert(name='skipper',ownerperson=1)
        row = db(db.person.id==db.dog.ownerperson).select().first()
        self.assertEqual(row[db.person.name],'max')
        self.assertEqual(row['person.name'],'max')
        drop(db.dog)
        self.assertEqual(len(db.person._referenced_by),0)
        drop(db.person)

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestRNameFields(unittest.TestCase):
    # tests for highly experimental rname attribute
    @unittest.skipIf(IS_GAE or IS_MONGODB, 'TODO: Datastore throws unsupported error for AGGREGATE. MongoDB assertion error (long object has no attribute id)')
    def testSelect(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        rname = db._adapter.QUOTE_TEMPLATE % 'a very complicated fieldname'
        rname2 = db._adapter.QUOTE_TEMPLATE % 'rrating from 1 to 10'
        db.define_table(
            'easy_name',
            Field('a_field', rname=rname),
            Field('rating', 'integer', rname=rname2, default=2)
            )
        rtn = db.easy_name.insert(a_field='a')
        self.assertEqual(isinstance(rtn.id, long), True)
        rtn = db(db.easy_name.a_field == 'a').select()
        self.assertEqual(len(rtn), 1)
        self.assertEqual(isinstance(rtn[0].id, long), True)
        self.assertEqual(rtn[0].a_field, 'a')
        db.easy_name.insert(a_field='b')
        rtn = db(db.easy_name.id > 0).delete()
        self.assertEqual(rtn, 2)
        rtn = db(db.easy_name.id > 0).count()
        self.assertEqual(rtn, 0)
        db.easy_name.insert(a_field='a')
        db.easy_name.insert(a_field='b')
        rtn = db(db.easy_name.id > 0).count()
        self.assertEqual(rtn, 2)
        rtn = db(db.easy_name.a_field == 'a').update(a_field='c')
        rtn = db(db.easy_name.a_field == 'c').count()
        self.assertEqual(rtn, 1)
        rtn = db(db.easy_name.a_field != 'c').count()
        self.assertEqual(rtn, 1)
        avg = db.easy_name.id.avg()
        rtn = db(db.easy_name.id > 0).select(avg)
        self.assertEqual(rtn[0][avg], 3)

        avg = db.easy_name.rating.avg()
        rtn = db(db.easy_name.id > 0).select(avg)
        self.assertEqual(rtn[0][avg], 2)

        rname = db._adapter.QUOTE_TEMPLATE % 'this is the person name'
        db.define_table(
            'person',
            Field('name', default="Michael", rname=rname),
            Field('uuid')
            )
        michael = db.person.insert() #default insert
        john = db.person.insert(name='John')
        luke = db.person.insert(name='Luke')

        self.assertEqual(len(rtn), 3)
        self.assertEqual(rtn[0].id, michael)
        self.assertEqual(rtn[0].name, 'Michael')
        self.assertEqual(rtn[1].id, john)
        self.assertEqual(rtn[1].name, 'John')
        #fetch owners, eventually with pet
        #main point is retrieving Luke with no pets
        rtn = db(db.person.id > 0).select()
        self.assertEqual(rtn[0].id, michael)
        self.assertEqual(rtn[0].name, 'Michael')
        self.assertEqual(rtn[3].name, 'Luke')
        self.assertEqual(rtn[3].id, luke)
        #as dict
        rtn = db(db.person.id > 0).select().as_dict()
        self.assertEqual(rtn[1]['name'], 'Michael')
        #as list
        rtn = db(db.person.id > 0).select().as_list()
        self.assertEqual(rtn[0]['name'], 'Michael')
        #isempty
        rtn = db(db.person.id > 0).isempty()
        self.assertEqual(rtn, False)

        #aliases
        rname = db._adapter.QUOTE_TEMPLATE % 'the cub name'
        if DEFAULT_URI.startswith('mssql'):
            #multiple cascade gotcha
            for key in ['reference','reference FK']:
                db._adapter.types[key]=db._adapter.types[key].replace(
                '%(on_delete_action)s','NO ACTION')
        db.define_table('pet_farm',
            Field('name', rname=rname),
            Field('father','reference pet_farm'),
            Field('mother','reference pet_farm'),
        )

        minali = db.pet_farm.insert(name='Minali')
        osbert = db.pet_farm.insert(name='Osbert')

        #they had a cub
        selina = db.pet_farm.insert(name='Selina', father=osbert, mother=minali)

        father = db.pet_farm.with_alias('father')
        mother = db.pet_farm.with_alias('mother')

        #fetch pets with relatives
        rtn = db().select(
            db.pet_farm.name, father.name, mother.name,
            left=[
                father.on(father.id == db.pet_farm.father),
                mother.on(mother.id == db.pet_farm.mother)
            ],
            orderby=db.pet_farm.id
        )

        self.assertEqual(len(rtn), 3)
        self.assertEqual(rtn[0].pet_farm.name, 'Minali')
        self.assertEqual(rtn[0].father.name, None)
        self.assertEqual(rtn[0].mother.name, None)
        self.assertEqual(rtn[1].pet_farm.name, 'Osbert')
        self.assertEqual(rtn[2].pet_farm.name, 'Selina')
        self.assertEqual(rtn[2].father.name, 'Osbert')
        self.assertEqual(rtn[2].mother.name, 'Minali')

        #clean up
        drop(db.pet_farm)
        drop(db.person)
        drop(db.easy_name)

    @unittest.skipIf(IS_GAE or IS_MONGODB, 'TODO: Datastore does not accept dict objects as json field input. MongoDB assertionerror Binary("x", 0) != "x"')
    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        rname = db._adapter.QUOTE_TEMPLATE % 'a very complicated fieldname'
        for ft in ['string', 'text', 'password', 'upload', 'blob']:
            db.define_table('tt', Field('aa', ft, default='', rname=rname))
            self.assertEqual(isinstance(db.tt.insert(aa='x'), long), True)
            self.assertEqual(db().select(db.tt.aa)[0].aa, 'x')
            drop(db.tt)
        db.define_table('tt', Field('aa', 'integer', default=1, rname=rname))
        self.assertEqual(isinstance(db.tt.insert(aa=3), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, 3)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'double', default=1, rname=rname))
        self.assertEqual(isinstance(db.tt.insert(aa=3.1), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, 3.1)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'boolean', default=True, rname=rname))
        self.assertEqual(isinstance(db.tt.insert(aa=True), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, True)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'json', default={}, rname=rname))
        self.assertEqual(isinstance(db.tt.insert(aa={}), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, {})
        drop(db.tt)
        db.define_table('tt', Field('aa', 'date',
                        default=datetime.date.today(), rname=rname))
        t0 = datetime.date.today()
        self.assertEqual(isinstance(db.tt.insert(aa=t0), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, t0)
        drop(db.tt)
        db.define_table('tt', Field('aa', 'datetime',
                        default=datetime.datetime.today(), rname=rname))
        t0 = datetime.datetime(
            1971,
            12,
            21,
            10,
            30,
            55,
            0,
            )
        self.assertEqual(db.tt.insert(aa=t0), 1)
        self.assertEqual(db().select(db.tt.aa)[0].aa, t0)

        ## Row APIs
        row = db().select(db.tt.aa)[0]
        self.assertEqual(db.tt[1].aa,t0)
        self.assertEqual(db.tt['aa'],db.tt.aa)
        self.assertEqual(db.tt(1).aa,t0)
        self.assertTrue(db.tt(1,aa=None)==None)
        self.assertFalse(db.tt(1,aa=t0)==None)
        self.assertEqual(row.aa,t0)
        self.assertEqual(row['aa'],t0)
        self.assertEqual(row['tt.aa'],t0)
        self.assertEqual(row('tt.aa'),t0)

        ## Lazy and Virtual fields
        db.tt.b = Field.Virtual(lambda row: row.tt.aa)
        db.tt.c = Field.Lazy(lambda row: row.tt.aa)
        row = db().select(db.tt.aa)[0]
        self.assertEqual(row.b,t0)
        self.assertEqual(row.c(),t0)

        drop(db.tt)
        db.define_table('tt', Field('aa', 'time', default='11:30', rname=rname))
        t0 = datetime.time(10, 30, 55)
        self.assertEqual(isinstance(db.tt.insert(aa=t0), long), True)
        self.assertEqual(db().select(db.tt.aa)[0].aa, t0)
        drop(db.tt)

    def testInsert(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        rname = db._adapter.QUOTE_TEMPLATE % 'a very complicated fieldname'
        db.define_table('tt', Field('aa', rname=rname))
        self.assertEqual(isinstance(db.tt.insert(aa='1'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa='1'), long), True)
        self.assertEqual(isinstance(db.tt.insert(aa='1'), long), True)
        self.assertEqual(db(db.tt.aa == '1').count(), 3)
        self.assertEqual(db(db.tt.aa == '2').isempty(), True)
        self.assertEqual(db(db.tt.aa == '1').update(aa='2'), 3)
        self.assertEqual(db(db.tt.aa == '2').count(), 3)
        self.assertEqual(db(db.tt.aa == '2').isempty(), False)
        self.assertEqual(db(db.tt.aa == '2').delete(), 3)
        self.assertEqual(db(db.tt.aa == '2').isempty(), True)
        drop(db.tt)

    @unittest.skip("JOIN queries are not supported")
    def testJoin(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        rname = db._adapter.QUOTE_TEMPLATE % 'this is field aa'
        rname2 = db._adapter.QUOTE_TEMPLATE % 'this is field b'
        db.define_table('t1', Field('aa', rname=rname))
        db.define_table('t2', Field('aa', rname=rname), Field('b', db.t1, rname=rname2))
        i1 = db.t1.insert(aa='1')
        i2 = db.t1.insert(aa='2')
        i3 = db.t1.insert(aa='3')
        db.t2.insert(aa='4', b=i1)
        db.t2.insert(aa='5', b=i2)
        db.t2.insert(aa='6', b=i2)
        self.assertEqual(len(db(db.t1.id
                          == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)), 3)
        self.assertEqual(db(db.t1.id == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)[2].t1.aa, '2')
        self.assertEqual(db(db.t1.id == db.t2.b).select(orderby=db.t1.aa
                          | db.t2.aa)[2].t2.aa, '6')
        self.assertEqual(len(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)), 4)
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[2].t1.aa, '2')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[2].t2.aa, '6')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[3].t1.aa, '3')
        self.assertEqual(db().select(db.t1.ALL, db.t2.ALL,
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa | db.t2.aa)[3].t2.aa, None)
        self.assertEqual(len(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa, groupby=db.t1.aa)),
                         3)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[0]._extra[db.t2.id.count()],
                         1)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[1]._extra[db.t2.id.count()],
                         2)
        self.assertEqual(db().select(db.t1.aa, db.t2.id.count(),
                         left=db.t2.on(db.t1.id == db.t2.b),
                         orderby=db.t1.aa,
                         groupby=db.t1.aa)[2]._extra[db.t2.id.count()],
                         0)
        drop(db.t2)
        drop(db.t1)

        db.define_table('person',Field('name', rname=rname))
        id = db.person.insert(name="max")
        self.assertEqual(id.name,'max')
        db.define_table('dog',Field('name', rname=rname),Field('ownerperson','reference person', rname=rname2))
        db.dog.insert(name='skipper',ownerperson=1)
        row = db(db.person.id==db.dog.ownerperson).select().first()
        self.assertEqual(row[db.person.name],'max')
        self.assertEqual(row['person.name'],'max')
        drop(db.dog)
        self.assertEqual(len(db.person._referenced_by),0)
        drop(db.person)

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestQuoting(unittest.TestCase):

    # tests for case sensitivity
    def testCase(self):
        return
        db = DAL(DEFAULT_URI, check_reserved=['all'], ignore_field_case=False)
        if DEFAULT_URI.startswith('mssql'):
            #multiple cascade gotcha
            for key in ['reference','reference FK']:
                db._adapter.types[key]=db._adapter.types[key].replace(
                '%(on_delete_action)s','NO ACTION')

        # test table case
        t0 = db.define_table('B',
                        Field('f', 'string'))
        try:
            t1 = db.define_table('b',
                                 Field('B', t0),
                                 Field('words', 'text'))
        except Exception, e:
            # An error is expected when database does not support case
            # sensitive entity names.
            if DEFAULT_URI.startswith('sqlite:'):
                self.assertTrue(isinstance(e, db._adapter.driver.OperationalError))
                return
            raise e

        blather = 'blah blah and so'
        t0[0] = {'f': 'content'}
        t1[0] = {'B': int(t0[1]['id']),
                 'words': blather}

        r = db(db.B.id==db.b.B).select()

        self.assertEqual(r[0].b.words, blather)

        drop(t1)
        drop(t0)

        # test field case
        try:
            t0 = db.define_table('table is a test',
                                 Field('a_a'),
                                 Field('a_A'))
        except Exception, e:
            # some db does not support case sensitive field names mysql is one of them.
            if DEFAULT_URI.startswith('mysql:'):
                db.rollback()
                return
            raise e

        t0[0] = dict(a_a = 'a_a', a_A='a_A')

        self.assertEqual(t0[1].a_a, 'a_a')
        self.assertEqual(t0[1].a_A, 'a_A')

        drop(t0)

    def testPKFK(self):

        # test primary keys

        db = DAL(DEFAULT_URI, check_reserved=['all'], ignore_field_case=False)
        if DEFAULT_URI.startswith('mssql'):
            #multiple cascade gotcha
            for key in ['reference','reference FK']:
                db._adapter.types[key]=db._adapter.types[key].replace(
                '%(on_delete_action)s','NO ACTION')
        # test table without surrogate key. Length must is limited to
        # 100 because of MySQL limitations: it cannot handle more than
        # 767 bytes in unique keys.

        t0 = db.define_table('t0', Field('Code', length=100), primarykey=['Code'])
        t2 = db.define_table('t2', Field('f'), Field('t0_Code', 'reference t0'))
        t3 = db.define_table('t3', Field('f', length=100), Field('t0_Code', t0.Code), primarykey=['f'])
        t4 = db.define_table('t4', Field('f', length=100), Field('t0', t0), primarykey=['f'])

        try:
            t5 = db.define_table('t5', Field('f', length=100), Field('t0', 'reference no_table_wrong_reference'), primarykey=['f'])
        except Exception, e:
            self.assertTrue(isinstance(e, KeyError))

        if DEFAULT_URI.startswith('mssql'):
            #there's no drop cascade in mssql
            drop(t3)
            drop(t4)
            drop(t2)
            drop(t0)
        else:
            drop(t0, 'cascade')
            drop(t2)
            drop(t3)
            drop(t4)


class TestTableAndFieldCase(unittest.TestCase):
    """
    at the Python level we should not allow db.C and db.c because of .table conflicts on windows
    but it should be possible to map two different names into distinct tables "c" and "C" at the Python level
    By default Python models names should be mapped into lower case table names and assume case insensitivity.
    """
    def testme(self):
        return


class TestQuotesByDefault(unittest.TestCase):
    """
    all default tables names should be quoted unless an explicit mapping has been given for a table.
    """
    def testme(self):
        return

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestRecordVersioning(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        tt = db.define_table('tt', Field('name'),
                        Field('is_active', 'boolean', default=True))
        db.tt._enable_record_versioning(archive_name='tt_archive')
        self.assertTrue('tt_archive' in db)
        i_id = db.tt.insert(name='web2py1')
        db.tt.insert(name='web2py2')
        db(db.tt.name == 'web2py2').delete()
        self.assertEqual(len(db(db.tt).select()), 1)
        self.assertEquals(db(db.tt).count(), 1)
        db(db.tt.id == i_id).update(name='web2py3')
        self.assertEqual(len(db(db.tt).select()), 1)
        self.assertEqual(db(db.tt).count(), 1)
        self.assertEqual(len(db(db.tt_archive).select()), 2)
        self.assertEqual(db(db.tt_archive).count(), 2)
        drop(db.tt_archive)
        # it allows tt to be dropped
        db.tt._before_delete = []
        drop(tt)
        return

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestBasicOps(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        tt = db.define_table('tt', Field('name'),
                        Field('is_active', 'boolean', default=True))
        i_id = db.tt.insert(name='web2py1')
        db.tt.insert(name='web2py2')
        db(db.tt.name == 'web2py2').delete()
        self.assertEqual(len(db(db.tt).select()), 1)
        self.assertEquals(db(db.tt).count(), 1)
        db(db.tt.id == i_id).update(name='web2py3')
        self.assertEqual(len(db(db.tt).select()), 1)
        self.assertEqual(db(db.tt).count(), 1)
        drop(tt)
        return

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestUpdateInsert(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        t0 = db.define_table('t0', Field('name'))
        i_id = db.t0.update_or_insert((db.t0.id == 1), name='web2py')
        u_id = db.t0.update_or_insert((db.t0.id == i_id), name='web2py2')
        self.assertTrue(i_id != None)
        self.assertTrue(u_id == None)
        self.assertEqual(len(db(db.t0).select()), 1)
        self.assertEqual(db(db.t0).count(), 1)
        self.assertEqual(db(db.t0.name == 'web2py').count(), 0)
        self.assertEqual(db(db.t0.name == 'web2py2').count(), 1)
        drop(t0)

@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestBulkInsert(unittest.TestCase):

    def testRun(self):
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        t0 = db.define_table('t0', Field('name'))
        global ctr
        ctr = 0
        def test_after_insert(i, r):
            global ctr
            ctr += 1
            return True
        t0._after_insert.append(test_after_insert)
        items = [{'name':'web2py_%s' % pos} for pos in range(0, 10, 1)]
        t0.bulk_insert(items)
        self.assertTrue(db(t0).count() == len(items))
        for pos in range(0, 10, 1):
            self.assertEqual(len(db(t0.name == 'web2py_%s' % pos).select()), 1)
            self.assertEqual(db(t0.name == 'web2py_%s' % pos).count(), 1)
        self.assertTrue(ctr == len(items))
        drop(t0)
        return


if __name__ == '__main__':
    unittest.main()
    tearDownModule()
