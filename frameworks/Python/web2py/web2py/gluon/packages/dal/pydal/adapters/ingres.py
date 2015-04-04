# -*- coding: utf-8 -*-

from .._globals import IDENTITY
from ..drivers import pyodbc
from .base import BaseAdapter

# NOTE invalid database object name (ANSI-SQL wants
#      this form of name to be a delimited identifier)
INGRES_SEQNAME='ii***lineitemsequence'


class IngresAdapter(BaseAdapter):
    drivers = ('pyodbc',)

    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR(%(length)s)',
        'text': 'CLOB',
        'json': 'CLOB',
        'password': 'VARCHAR(%(length)s)',  ## Not sure what this contains utf8 or nvarchar. Or even bytes?
        'blob': 'BLOB',
        'upload': 'VARCHAR(%(length)s)',  ## FIXME utf8 or nvarchar... or blob? what is this type?
        'integer': 'INTEGER4', # or int8...
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT8',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'ANSIDATE',
        'time': 'TIME WITHOUT TIME ZONE',
        'datetime': 'TIMESTAMP WITHOUT TIME ZONE',
        'id': 'int not null unique with default next value for %s' % INGRES_SEQNAME,
        'reference': 'INT, FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'CLOB',
        'list:string': 'CLOB',
        'list:reference': 'CLOB',
        'big-id': 'bigint not null unique with default next value for %s' % INGRES_SEQNAME,
        'big-reference': 'BIGINT, FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s', ## FIXME TODO
        }

    def LEFT_JOIN(self):
        return 'LEFT OUTER JOIN'

    def RANDOM(self):
        return 'RANDOM()'

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            fetch_amt = lmax - lmin
            if fetch_amt:
                sql_s += ' FIRST %d ' % (fetch_amt, )
            if lmin:
                # Requires Ingres 9.2+
                sql_o += ' OFFSET %d' % (lmin, )
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "ingres"
        self._driver = pyodbc
        self.uri = uri
        if do_connect: self.find_driver(adapter_args,uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        connstr = uri.split(':', 1)[1]
        # Simple URI processing
        connstr = connstr.lstrip()
        while connstr.startswith('/'):
            connstr = connstr[1:]
        if '=' in connstr:
            # Assume we have a regular ODBC connection string and just use it
            ruri  = connstr
        else:
            # Assume only (local) dbname is passed in with OS auth
            database_name = connstr
            default_driver_name = 'Ingres'
            vnode = '(local)'
            servertype = 'ingres'
            ruri = 'Driver={%s};Server=%s;Database=%s' % (default_driver_name, vnode, database_name)
        def connector(cnxn=ruri,driver_args=driver_args):
            return self.driver.connect(cnxn,**driver_args)

        self.connector = connector

        # TODO if version is >= 10, set types['id'] to Identity column, see http://community.actian.com/wiki/Using_Ingres_Identity_Columns
        if do_connect: self.reconnect()

    def create_sequence_and_triggers(self, query, table, **args):
        # post create table auto inc code (if needed)
        # modify table to btree for performance....
        # Older Ingres releases could use rule/trigger like Oracle above.
        if hasattr(table,'_primarykey'):
            modify_tbl_sql = 'modify %s to btree unique on %s' % \
                (table._tablename,
                 ', '.join(["'%s'" % x for x in table.primarykey]))
            self.execute(modify_tbl_sql)
        else:
            tmp_seqname='%s_iisq' % table._tablename
            query=query.replace(INGRES_SEQNAME, tmp_seqname)
            self.execute('create sequence %s' % tmp_seqname)
            self.execute(query)
            self.execute('modify %s to btree unique on %s' % (table._tablename, 'id'))


    def lastrowid(self,table):
        tmp_seqname='%s_iisq' % table
        self.execute('select current value for %s' % tmp_seqname)
        return long(self.cursor.fetchone()[0]) # don't really need int type cast here...


class IngresUnicodeAdapter(IngresAdapter):

    drivers = ('pyodbc',)

    types = {
        'boolean': 'CHAR(1)',
        'string': 'NVARCHAR(%(length)s)',
        'text': 'NCLOB',
        'json': 'NCLOB',
        'password': 'NVARCHAR(%(length)s)',  ## Not sure what this contains utf8 or nvarchar. Or even bytes?
        'blob': 'BLOB',
        'upload': 'VARCHAR(%(length)s)',  ## FIXME utf8 or nvarchar... or blob? what is this type?
        'integer': 'INTEGER4', # or int8...
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT8',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'ANSIDATE',
        'time': 'TIME WITHOUT TIME ZONE',
        'datetime': 'TIMESTAMP WITHOUT TIME ZONE',
        'id': 'INTEGER4 not null unique with default next value for %s'% INGRES_SEQNAME,
        'reference': 'INTEGER4, FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'NCLOB',
        'list:string': 'NCLOB',
        'list:reference': 'NCLOB',
        'big-id': 'BIGINT not null unique with default next value for %s'% INGRES_SEQNAME,
        'big-reference': 'BIGINT, FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s', ## FIXME TODO
        }
