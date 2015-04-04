# -*- coding: utf-8 -*-

from .._globals import IDENTITY
from ..connection import ConnectionPool
from .base import BaseAdapter


class TeradataAdapter(BaseAdapter):
    drivers = ('pyodbc',)

    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR(%(length)s)',
        'text': 'VARCHAR(2000)',
        'json': 'VARCHAR(4000)',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'BLOB',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'REAL',
        'double': 'DOUBLE',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'TIME',
        'datetime': 'TIMESTAMP',
        # Modified Constraint syntax for Teradata.
        # Teradata does not support ON DELETE.
        'id': 'INT GENERATED ALWAYS AS IDENTITY',  # Teradata Specific
        'reference': 'INT',
        'list:integer': 'VARCHAR(4000)',
        'list:string': 'VARCHAR(4000)',
        'list:reference': 'VARCHAR(4000)',
        'geometry': 'ST_GEOMETRY',
        'big-id': 'BIGINT GENERATED ALWAYS AS IDENTITY',  # Teradata Specific
        'big-reference': 'BIGINT',
        'reference FK': ' REFERENCES %(foreign_key)s',
        'reference TFK': ' FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s)',
        }

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "teradata"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args,uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        ruri = uri.split('://', 1)[1]
        def connector(cnxn=ruri,driver_args=driver_args):
            return self.driver.connect(cnxn,**driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def close(self,action='commit',really=True):
        # Teradata does not implicitly close off the cursor
        # leading to SQL_ACTIVE_STATEMENTS limit errors
        self.cursor.close()
        ConnectionPool.close(self, action, really)

    def LEFT_JOIN(self):
        return 'LEFT OUTER JOIN'

    # Similar to MSSQL, Teradata can't specify a range (for Pageby)
    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            sql_s += ' TOP %i' % lmax
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)

    def _truncate(self, table, mode=''):
        tablename = table._tablename
        return ['DELETE FROM %s ALL;' % (tablename)]
