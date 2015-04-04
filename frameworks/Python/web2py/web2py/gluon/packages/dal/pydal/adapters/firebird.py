# -*- coding: utf-8 -*-
import re

from .._globals import IDENTITY
from ..objects import Expression
from .base import BaseAdapter


class FireBirdAdapter(BaseAdapter):
    drivers = ('kinterbasdb','firebirdsql','fdb','pyodbc')

    commit_on_alter_table = False
    support_distributed_transaction = True
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
        'decimal': 'DECIMAL(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'TIME',
        'datetime': 'TIMESTAMP',
        'id': 'INTEGER PRIMARY KEY',
        'reference': 'INTEGER REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'BLOB SUB_TYPE 1',
        'list:string': 'BLOB SUB_TYPE 1',
        'list:reference': 'BLOB SUB_TYPE 1',
        'big-id': 'BIGINT PRIMARY KEY',
        'big-reference': 'BIGINT REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        }

    def sequence_name(self,tablename):
        return ('genid_' + self.QUOTE_TEMPLATE) % tablename

    def trigger_name(self,tablename):
        return 'trg_id_%s' % tablename

    def RANDOM(self):
        return 'RAND()'

    def EPOCH(self, first):
        return "DATEDIFF(second, '1970-01-01 00:00:00', %s)" % self.expand(first)

    def NOT_NULL(self,default,field_type):
        return 'DEFAULT %s NOT NULL' % self.represent(default,field_type)

    def SUBSTRING(self,field,parameters):
        return 'SUBSTRING(%s from %s for %s)' % (self.expand(field), parameters[0], parameters[1])

    def LENGTH(self, first):
        return "CHAR_LENGTH(%s)" % self.expand(first)

    def CONTAINS(self,first,second,case_sensitive=False):
        if first.type.startswith('list:'):
            second = Expression(None,self.CONCAT('|',Expression(
                        None,self.REPLACE(second,('|','||'))),'|'))
        return '(%s CONTAINING %s)' % (self.expand(first),
                                       self.expand(second, 'string'))

    def _drop(self,table,mode):
        sequence_name = table._sequence_name
        return ['DROP TABLE %s %s;' % (table.sqlsafe, mode), 'DROP GENERATOR %s;' % sequence_name]

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            sql_s = ' FIRST %i SKIP %i %s' % (lmax - lmin, lmin, sql_s)
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)

    def _truncate(self,table,mode = ''):
        return ['DELETE FROM %s;' % table._tablename,
                'SET GENERATOR %s TO 0;' % table._sequence_name]

    REGEX_URI = re.compile('^(?P<user>[^:@]+)(\:(?P<password>[^@]*))?@(?P<host>[^\:/]+)(\:(?P<port>[0-9]+))?/(?P<db>.+?)(\?set_encoding=(?P<charset>\w+))?$')

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "firebird"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args,uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        ruri = uri.split('://',1)[1]
        m = self.REGEX_URI.match(ruri)
        if not m:
            raise SyntaxError("Invalid URI string in DAL: %s" % self.uri)
        user = credential_decoder(m.group('user'))
        if not user:
            raise SyntaxError('User required')
        password = credential_decoder(m.group('password'))
        if not password:
            password = ''
        host = m.group('host')
        if not host:
            raise SyntaxError('Host name required')
        port = int(m.group('port') or 3050)
        db = m.group('db')
        if not db:
            raise SyntaxError('Database name required')
        charset = m.group('charset') or 'UTF8'
        driver_args.update(dsn='%s/%s:%s' % (host,port,db),
                           user = credential_decoder(user),
                           password = credential_decoder(password),
                           charset = charset)

        def connector(driver_args=driver_args):
            return self.driver.connect(**driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def create_sequence_and_triggers(self, query, table, **args):
        tablename = table._tablename
        sequence_name = table._sequence_name
        trigger_name = table._trigger_name
        self.execute(query)
        self.execute('create generator %s;' % sequence_name)
        self.execute('set generator %s to 0;' % sequence_name)
        self.execute('create trigger %s for %s active before insert position 0 as\nbegin\nif(new.id is null) then\nbegin\nnew.id = gen_id(%s, 1);\nend\nend;' % (trigger_name, tablename, sequence_name))

    def lastrowid(self,table):
        sequence_name = table._sequence_name
        self.execute('SELECT gen_id(%s, 0) FROM rdb$database' % sequence_name)
        return long(self.cursor.fetchone()[0])


class FireBirdEmbeddedAdapter(FireBirdAdapter):
    drivers = ('kinterbasdb','firebirdsql','fdb','pyodbc')

    REGEX_URI = re.compile('^(?P<user>[^:@]+)(\:(?P<password>[^@]*))?@(?P<path>[^\?]+)(\?set_encoding=(?P<charset>\w+))?$')

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "firebird"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args,uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
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
        pathdb = m.group('path')
        if not pathdb:
            raise SyntaxError('Path required')
        charset = m.group('charset')
        if not charset:
            charset = 'UTF8'
        host = ''
        driver_args.update(host=host,
                           database=pathdb,
                           user=credential_decoder(user),
                           password=credential_decoder(password),
                           charset=charset)

        def connector(driver_args=driver_args):
            return self.driver.connect(**driver_args)
        self.connector = connector
        if do_connect: self.reconnect()
