# -*- coding: utf-8 -*-
import base64
import datetime

from .._globals import IDENTITY
from .base import BaseAdapter


class DB2Adapter(BaseAdapter):
    drivers = ('ibm_db_dbi', 'pyodbc')

    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR(%(length)s)',
        'text': 'CLOB',
        'json': 'CLOB',
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
        'id': 'INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY NOT NULL',
        'reference': 'INT, FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'CLOB',
        'list:string': 'CLOB',
        'list:reference': 'CLOB',
        'big-id': 'BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY NOT NULL',
        'big-reference': 'BIGINT, FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
        }

    def LEFT_JOIN(self):
        return 'LEFT OUTER JOIN'

    def RANDOM(self):
        return 'RAND()'

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            sql_o += ' FETCH FIRST %i ROWS ONLY' % lmax
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)

    def represent_exceptions(self, obj, fieldtype):
        if fieldtype == 'blob':
            obj = base64.b64encode(str(obj))
            return "BLOB('%s')" % obj
        elif fieldtype == 'datetime':
            if isinstance(obj, datetime.datetime):
                obj = obj.isoformat()[:19].replace('T','-').replace(':','.')
            elif isinstance(obj, datetime.date):
                obj = obj.isoformat()[:10]+'-00.00.00'
            return "'%s'" % obj
        return None

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "db2"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args,uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        self.test_query = 'SELECT 1 FROM (VALUES ( 1 ));'
        ruri = uri.split('://', 1)[1]

        
        def connector(cnxn=ruri,driver_args=driver_args):
            if self.driver_name == 'ibm_db_dbi':
                vars = cnxn.split(";")
                cnxn = {}
                for var in vars:
                    v = var.split('=')
                    cnxn[v[0].lower()] = v[1]
                return self.driver.connect(cnxn['dsn'], cnxn['uid'], cnxn['pwd'], **driver_args)
            else:
                return self.driver.connect(cnxn, **driver_args)
                
        self.connector = connector
        if do_connect: self.reconnect()

    def execute(self,command,placeholders=None):
        if command[-1:]==';':
            command = command[:-1]
        if placeholders:
            return self.log_execute(command, placeholders)
        return self.log_execute(command)

    def lastrowid(self,table):
        self.execute('SELECT DISTINCT IDENTITY_VAL_LOCAL() FROM %s;' % table)
        return long(self.cursor.fetchone()[0])

    def rowslice(self,rows,minimum=0,maximum=None):
        if maximum is None:
            return rows[minimum:]
        return rows[minimum:maximum]
