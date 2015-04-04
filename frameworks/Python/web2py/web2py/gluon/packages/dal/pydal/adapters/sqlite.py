# -*- coding: utf-8 -*-
import copy
import datetime
import locale
import platform
import re
import sys
import time

from .._compat import PY2, pjoin
from .._globals import IDENTITY
from .base import BaseAdapter


class SQLiteAdapter(BaseAdapter):
    drivers = ('sqlite2','sqlite3')

    can_select_for_update = None    # support ourselves with BEGIN TRANSACTION

    def EXTRACT(self,field,what):
        return "web2py_extract('%s',%s)" % (what, self.expand(field))

    @staticmethod
    def web2py_extract(lookup, s):
        table = {
            'year': (0, 4),
            'month': (5, 7),
            'day': (8, 10),
            'hour': (11, 13),
            'minute': (14, 16),
            'second': (17, 19),
            }
        try:
            if lookup != 'epoch':
                (i, j) = table[lookup]
                return int(s[i:j])
            else:
                return time.mktime(datetime.datetime.strptime(s, '%Y-%m-%d %H:%M:%S').timetuple())
        except:
            return None

    @staticmethod
    def web2py_regexp(expression, item):
        return re.compile(expression).search(item) is not None

    def __init__(self, db, uri, pool_size=0, folder=None, db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "sqlite"
        self.uri = uri
        self.adapter_args = adapter_args
        if do_connect: self.find_driver(adapter_args)
        self.pool_size = 0
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        path_encoding = sys.getfilesystemencoding() \
            or locale.getdefaultlocale()[1] or 'utf8'
        if uri.startswith('sqlite:memory'):
            self.dbpath = ':memory:'
        else:
            self.dbpath = uri.split('://',1)[1]
            if self.dbpath[0] != '/':
                if PY2:
                    self.dbpath = pjoin(
                        self.folder.decode(path_encoding).encode('utf8'), self.dbpath)
                else:
                    self.dbpath = pjoin(self.folder, self.dbpath)
        if not 'check_same_thread' in driver_args:
            driver_args['check_same_thread'] = False
        if not 'detect_types' in driver_args and do_connect:
            driver_args['detect_types'] = self.driver.PARSE_DECLTYPES
        def connector(dbpath=self.dbpath, driver_args=driver_args):
            return self.driver.Connection(dbpath, **driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def after_connection(self):
        self.connection.create_function('web2py_extract', 2,
                                        SQLiteAdapter.web2py_extract)
        self.connection.create_function("REGEXP", 2,
                                        SQLiteAdapter.web2py_regexp)

        if self.adapter_args.get('foreign_keys',True):
            self.execute('PRAGMA foreign_keys=ON;')

    def _truncate(self, table, mode=''):
        tablename = table._tablename
        return ['DELETE FROM %s;' % tablename,
                "DELETE FROM sqlite_sequence WHERE name='%s';" % tablename]

    def lastrowid(self, table):
        return self.cursor.lastrowid

    def REGEXP(self,first,second):
        return '(%s REGEXP %s)' % (self.expand(first),
                                   self.expand(second,'string'))
    
    def delete(self, tablename, query):
        # SQLite requires its own delete to handle CASCADE
        db = self.db
        table = db[tablename]
        deleted = [x[table._id.name] for x in db(query).select(table._id)]

        counter = super(SQLiteAdapter, self).delete(tablename, query)

        if counter:
            for field in table._referenced_by:
                if field.type == 'reference '+ tablename \
                        and field.ondelete == 'CASCADE':
                    db(field.belongs(deleted)).delete()

        return counter

    def select(self, query, fields, attributes):
        """
        Simulate `SELECT ... FOR UPDATE` with `BEGIN IMMEDIATE TRANSACTION`.
        Note that the entire database, rather than one record, is locked
        (it will be locked eventually anyway by the following UPDATE).
        """
        if attributes.get('for_update', False) and not 'cache' in attributes:
            self.execute('BEGIN IMMEDIATE TRANSACTION;')
        return super(SQLiteAdapter, self).select(query, fields, attributes)


SPATIALLIBS = {
    'Windows':'libspatialite',
    'Linux':'libspatialite.so',
    'Darwin':'libspatialite.dylib'
    }

class SpatiaLiteAdapter(SQLiteAdapter):
    drivers = ('sqlite3','sqlite2')

    types = copy.copy(BaseAdapter.types)
    types.update(geometry='GEOMETRY')

    def __init__(self, db, uri, pool_size=0, folder=None, db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, srid=4326, after_connection=None):
        self.db = db
        self.dbengine = "spatialite"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args)
        self.pool_size = 0
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        self.srid = srid
        path_encoding = sys.getfilesystemencoding() \
            or locale.getdefaultlocale()[1] or 'utf8'
        if uri.startswith('spatialite:memory'):
            self.dbpath = ':memory:'
        else:
            self.dbpath = uri.split('://',1)[1]
            if self.dbpath[0] != '/':
                self.dbpath = pjoin(
                    self.folder.decode(path_encoding).encode('utf8'), self.dbpath)
        if not 'check_same_thread' in driver_args:
            driver_args['check_same_thread'] = False
        if not 'detect_types' in driver_args and do_connect:
            driver_args['detect_types'] = self.driver.PARSE_DECLTYPES
        def connector(dbpath=self.dbpath, driver_args=driver_args):
            return self.driver.Connection(dbpath, **driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def after_connection(self):
        self.connection.enable_load_extension(True)
        # for Windows, rename libspatialite-2.dll to libspatialite.dll
        # Linux uses libspatialite.so
        # Mac OS X uses libspatialite.dylib
        libspatialite = SPATIALLIBS[platform.system()]
        self.execute(r'SELECT load_extension("%s");' % libspatialite)

        self.connection.create_function('web2py_extract', 2,
                                        SQLiteAdapter.web2py_extract)
        self.connection.create_function("REGEXP", 2,
                                        SQLiteAdapter.web2py_regexp)

    # GIS functions

    def ST_ASGEOJSON(self, first, second):
        return 'AsGeoJSON(%s,%s,%s)' %(self.expand(first),
            second['precision'], second['options'])

    def ST_ASTEXT(self, first):
        return 'AsText(%s)' %(self.expand(first))

    def ST_CONTAINS(self, first, second):
        return 'Contains(%s,%s)' %(self.expand(first),
                                   self.expand(second, first.type))

    def ST_DISTANCE(self, first, second):
        return 'Distance(%s,%s)' %(self.expand(first),
                                   self.expand(second, first.type))

    def ST_EQUALS(self, first, second):
        return 'Equals(%s,%s)' %(self.expand(first),
                                 self.expand(second, first.type))

    def ST_INTERSECTS(self, first, second):
        return 'Intersects(%s,%s)' %(self.expand(first),
                                     self.expand(second, first.type))

    def ST_OVERLAPS(self, first, second):
        return 'Overlaps(%s,%s)' %(self.expand(first),
                                   self.expand(second, first.type))

    def ST_SIMPLIFY(self, first, second):
        return 'Simplify(%s,%s)' %(self.expand(first),
                                   self.expand(second, 'double'))

    def ST_TOUCHES(self, first, second):
        return 'Touches(%s,%s)' %(self.expand(first),
                                  self.expand(second, first.type))

    def ST_WITHIN(self, first, second):
        return 'Within(%s,%s)' %(self.expand(first),
                                 self.expand(second, first.type))

    def represent(self, obj, fieldtype):
        field_is_type = fieldtype.startswith
        if field_is_type('geo'):
            srid = 4326 # Spatialite default srid for geometry
            geotype, parms = fieldtype[:-1].split('(')
            parms = parms.split(',')
            if len(parms) >= 2:
                schema, srid = parms[:2]
#             if field_is_type('geometry'):
            value = "ST_GeomFromText('%s',%s)" %(obj, srid)
#             elif field_is_type('geography'):
#                 value = "ST_GeogFromText('SRID=%s;%s')" %(srid, obj)
#             else:
#                 raise SyntaxError, 'Invalid field type %s' %fieldtype
            return value
        return BaseAdapter.represent(self, obj, fieldtype)


class JDBCSQLiteAdapter(SQLiteAdapter):
    drivers = ('zxJDBC_sqlite',)

    def __init__(self, db, uri, pool_size=0, folder=None, db_codec='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "sqlite"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        path_encoding = sys.getfilesystemencoding() \
            or locale.getdefaultlocale()[1] or 'utf8'
        if uri.startswith('sqlite:memory'):
            self.dbpath = ':memory:'
        else:
            self.dbpath = uri.split('://',1)[1]
            if self.dbpath[0] != '/':
                self.dbpath = pjoin(
                    self.folder.decode(path_encoding).encode('utf8'), self.dbpath)
        def connector(dbpath=self.dbpath,driver_args=driver_args):
            return self.driver.connect(
                self.driver.getConnection('jdbc:sqlite:'+dbpath),
                **driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def after_connection(self):
        # FIXME http://www.zentus.com/sqlitejdbc/custom_functions.html for UDFs
        self.connection.create_function('web2py_extract', 2,
                                        SQLiteAdapter.web2py_extract)

    def execute(self, a):
        return self.log_execute(a)
