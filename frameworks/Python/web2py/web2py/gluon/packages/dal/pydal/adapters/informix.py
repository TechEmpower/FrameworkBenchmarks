# -*- coding: utf-8 -*-
import datetime
import re

from .._globals import IDENTITY
from .base import BaseAdapter


class InformixAdapter(BaseAdapter):
    drivers = ('informixdb',)

    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR(%(length)s)',
        'text': 'BLOB SUB_TYPE 1',
        'json': 'BLOB SUB_TYPE 1',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'BLOB SUB_TYPE 0',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INTEGER',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'DOUBLE PRECISION',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'CHAR(8)',
        'datetime': 'DATETIME',
        'id': 'SERIAL',
        'reference': 'INTEGER REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'BLOB SUB_TYPE 1',
        'list:string': 'BLOB SUB_TYPE 1',
        'list:reference': 'BLOB SUB_TYPE 1',
        'big-id': 'BIGSERIAL',
        'big-reference': 'BIGINT REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': 'REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s CONSTRAINT FK_%(table_name)s_%(field_name)s',
        'reference TFK': 'FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s CONSTRAINT TFK_%(table_name)s_%(field_name)s',
        }

    def RANDOM(self):
        return 'Random()'

    def NOT_NULL(self,default,field_type):
        return 'DEFAULT %s NOT NULL' % self.represent(default,field_type)

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            fetch_amt = lmax - lmin
            dbms_version = int(self.connection.dbms_version.split('.')[0])
            if lmin and (dbms_version >= 10):
                # Requires Informix 10.0+
                sql_s += ' SKIP %d' % (lmin, )
            if fetch_amt and (dbms_version >= 9):
                # Requires Informix 9.0+
                sql_s += ' FIRST %d' % (fetch_amt, )
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)

    def represent_exceptions(self, obj, fieldtype):
        if fieldtype == 'date':
            if isinstance(obj, (datetime.date, datetime.datetime)):
                obj = obj.isoformat()[:10]
            else:
                obj = str(obj)
            return "to_date('%s','%%Y-%%m-%%d')" % obj
        elif fieldtype == 'datetime':
            if isinstance(obj, datetime.datetime):
                obj = obj.isoformat()[:19].replace('T',' ')
            elif isinstance(obj, datetime.date):
                obj = obj.isoformat()[:10]+' 00:00:00'
            else:
                obj = str(obj)
            return "to_date('%s','%%Y-%%m-%%d %%H:%%M:%%S')" % obj
        return None

    REGEX_URI = re.compile('^(?P<user>[^:@]+)(\:(?P<password>[^@]*))?@(?P<host>[^\:/]+)(\:(?P<port>[0-9]+))?/(?P<db>.+)$')

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "informix"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args,uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        self.test_query = 'SELECT COUNT(*) FROM systables;'
        ruri = uri.split('://',1)[1]
        m = self.REGEX_URI.match(ruri)
        if not m:
            raise SyntaxError(
                "Invalid URI string in DAL: %s" % self.uri)
        user = credential_decoder(m.group('user'))
        if not user:
            raise SyntaxError('User required')
        password = credential_decoder(m.group('password'))
        if not password:
            password = ''
        host = m.group('host')
        if not host:
            raise SyntaxError('Host name required')
        db = m.group('db')
        if not db:
            raise SyntaxError('Database name required')
        user = credential_decoder(user)
        password = credential_decoder(password)
        dsn = '%s@%s' % (db,host)
        driver_args.update(user=user,password=password)
        def connector(dsn=dsn,driver_args=driver_args):
            return self.driver.connect(dsn,**driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def execute(self,command):
        if command[-1:]==';':
            command = command[:-1]
        return self.log_execute(command)

    def lastrowid(self,table):
        return self.cursor.sqlerrd[1]


class InformixSEAdapter(InformixAdapter):
    """ work in progress """

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        return 'SELECT %s %s FROM %s%s%s;' % \
            (sql_s, sql_f, sql_t, sql_w, sql_o)

    def rowslice(self,rows,minimum=0,maximum=None):
        if maximum is None:
            return rows[minimum:]
        return rows[minimum:maximum]
