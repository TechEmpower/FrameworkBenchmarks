# -*- coding: utf-8 -*-
import re

from .._globals import IDENTITY
from .base import BaseAdapter


class SAPDBAdapter(BaseAdapter):
    drivers = ('sapdb',)

    support_distributed_transaction = False
    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR(%(length)s)',
        'text': 'LONG',
        'json': 'LONG',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'LONG',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'DOUBLE PRECISION',
        'decimal': 'FIXED(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'TIME',
        'datetime': 'TIMESTAMP',
        'id': 'INT PRIMARY KEY',
        'reference': 'INT, FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'LONG',
        'list:string': 'LONG',
        'list:reference': 'LONG',
        'big-id': 'BIGINT PRIMARY KEY',
        'big-reference': 'BIGINT, FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        }

    def sequence_name(self,table):
        return (self.QUOTE_TEMPLATE + '_id_Seq') % table

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            if len(sql_w) > 1:
                sql_w_row = sql_w + ' AND w_row > %i' % lmin
            else:
                sql_w_row = 'WHERE w_row > %i' % lmin
            return '%s %s FROM (SELECT w_tmp.*, ROWNO w_row FROM (SELECT %s FROM %s%s%s) w_tmp WHERE ROWNO=%i) %s %s %s;' % (sql_s, sql_f, sql_f, sql_t, sql_w, sql_o, lmax, sql_t, sql_w_row, sql_o)
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)

    def create_sequence_and_triggers(self, query, table, **args):
        # following lines should only be executed if table._sequence_name does not exist
        self.execute('CREATE SEQUENCE %s;' % table._sequence_name)
        self.execute("ALTER TABLE %s ALTER COLUMN %s SET DEFAULT NEXTVAL('%s');" \
                         % (table._tablename, table._id.name, table._sequence_name))
        self.execute(query)

    REGEX_URI = re.compile('^(?P<user>[^:@]+)(\:(?P<password>[^@]*))?@(?P<host>[^\:@]+)(\:(?P<port>[0-9]+))?/(?P<db>[^\?]+)(\?sslmode=(?P<sslmode>.+))?$')


    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "sapdb"
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
            raise SyntaxError("Invalid URI string in DAL")
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
        def connector(user=user, password=password, database=db,
                    host=host, driver_args=driver_args):
            return self.driver.Connection(user, password, database,
                                          host, **driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def lastrowid(self,table):
        self.execute("select %s.NEXTVAL from dual" % table._sequence_name)
        return long(self.cursor.fetchone()[0])
