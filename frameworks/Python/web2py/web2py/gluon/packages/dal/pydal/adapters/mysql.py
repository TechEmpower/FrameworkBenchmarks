# -*- coding: utf-8 -*-
import re

from .._globals import IDENTITY
from ..helpers.methods import varquote_aux
from .base import BaseAdapter


class MySQLAdapter(BaseAdapter):
    drivers = ('MySQLdb','pymysql', 'mysqlconnector')

    commit_on_alter_table = True
    support_distributed_transaction = True
    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR(%(length)s)',
        'text': 'LONGTEXT',
        'json': 'LONGTEXT',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'LONGBLOB',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'DOUBLE',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'TIME',
        'datetime': 'DATETIME',
        'id': 'INT AUTO_INCREMENT NOT NULL',
        'reference': 'INT, INDEX %(index_name)s (%(field_name)s), FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'LONGTEXT',
        'list:string': 'LONGTEXT',
        'list:reference': 'LONGTEXT',
        'big-id': 'BIGINT AUTO_INCREMENT NOT NULL',
        'big-reference': 'BIGINT, INDEX %(index_name)s (%(field_name)s), FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT  `FK_%(constraint_name)s` FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        }

    QUOTE_TEMPLATE = "`%s`"

    def varquote(self,name):
        return varquote_aux(name,'`%s`')

    def RANDOM(self):
        return 'RAND()'

    def SUBSTRING(self,field,parameters):
        return 'SUBSTRING(%s,%s,%s)' % (self.expand(field),
                                        parameters[0], parameters[1])

    def EPOCH(self, first):
        return "UNIX_TIMESTAMP(%s)" % self.expand(first)

    def CONCAT(self, *items):
        return 'CONCAT(%s)' % ','.join(self.expand(x,'string') for x in items)

    def REGEXP(self,first,second):
        return '(%s REGEXP %s)' % (self.expand(first),
                                   self.expand(second,'string'))

    def CAST(self, first, second):
        if second=='LONGTEXT': second = 'CHAR'
        return 'CAST(%s AS %s)' % (first, second)

    def _drop(self,table,mode):
        # breaks db integrity but without this mysql does not drop table
        table_rname = table.sqlsafe
        return ['SET FOREIGN_KEY_CHECKS=0;','DROP TABLE %s;' % table_rname,
                'SET FOREIGN_KEY_CHECKS=1;']

    def _insert_empty(self, table):
        return 'INSERT INTO %s VALUES (DEFAULT);' % (table.sqlsafe)

    def distributed_transaction_begin(self,key):
        self.execute('XA START;')

    def prepare(self,key):
        self.execute("XA END;")
        self.execute("XA PREPARE;")

    def commit_prepared(self,key):
        self.execute("XA COMMIT;")

    def rollback_prepared(self,key):
        self.execute("XA ROLLBACK;")

    REGEX_URI = re.compile('^(?P<user>[^:@]+)(\:(?P<password>[^@]*))?@(?P<host>[^\:/]+)(\:(?P<port>[0-9]+))?/(?P<db>[^?]+)(\?set_encoding=(?P<charset>\w+))?$')

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "mysql"
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
        host = m.group('host')
        if not host:
            raise SyntaxError('Host name required')
        db = m.group('db')
        if not db:
            raise SyntaxError('Database name required')
        port = int(m.group('port') or '3306')
        charset = m.group('charset') or 'utf8'
        driver_args.update(db=db,
                           user=credential_decoder(user),
                           passwd=credential_decoder(password),
                           host=host,
                           port=port,
                           charset=charset)


        def connector(driver_args=driver_args):
            return self.driver.connect(**driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def after_connection(self):
        self.execute('SET FOREIGN_KEY_CHECKS=1;')
        self.execute("SET sql_mode='NO_BACKSLASH_ESCAPES';")

    def lastrowid(self,table):
        self.execute('select last_insert_id();')
        return int(self.cursor.fetchone()[0])
