# -*- coding: utf-8 -*-
import base64
import datetime
import re

from .._globals import IDENTITY
from ..drivers import cx_Oracle
from .base import BaseAdapter


class OracleAdapter(BaseAdapter):
    drivers = ('cx_Oracle',)

    commit_on_alter_table = False
    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR2(%(length)s)',
        'text': 'CLOB',
        'json': 'CLOB',
        'password': 'VARCHAR2(%(length)s)',
        'blob': 'CLOB',
        'upload': 'VARCHAR2(%(length)s)',
        'integer': 'INT',
        'bigint': 'NUMBER',
        'float': 'FLOAT',
        'double': 'BINARY_DOUBLE',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'CHAR(8)',
        'datetime': 'DATE',
        'id': 'NUMBER PRIMARY KEY',
        'reference': 'NUMBER, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'CLOB',
        'list:string': 'CLOB',
        'list:reference': 'CLOB',
        'big-id': 'NUMBER PRIMARY KEY',
        'big-reference': 'NUMBER, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
        }


    def trigger_name(self,tablename):
        return '%s_trigger' % tablename

    def LEFT_JOIN(self):
        return 'LEFT OUTER JOIN'

    def RANDOM(self):
        return 'dbms_random.value'

    def NOT_NULL(self,default,field_type):
        return 'DEFAULT %s NOT NULL' % self.represent(default,field_type)

    def REGEXP(self, first, second):
        return 'REGEXP_LIKE(%s, %s)' % (self.expand(first),
                                        self.expand(second, 'string'))

    def _drop(self,table,mode):
        sequence_name = table._sequence_name
        return ['DROP TABLE %s %s;' % (table.sqlsafe, mode), 'DROP SEQUENCE %s;' % sequence_name]

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            if len(sql_w) > 1:
                sql_w_row = sql_w + ' AND w_row > %i' % lmin
            else:
                sql_w_row = 'WHERE w_row > %i' % lmin
            return 'SELECT %s %s FROM (SELECT w_tmp.*, ROWNUM w_row FROM (SELECT %s FROM %s%s%s) w_tmp WHERE ROWNUM<=%i) %s %s %s;' % (sql_s, sql_f, sql_f, sql_t, sql_w, sql_o, lmax, sql_t, sql_w_row, sql_o)
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)

    def constraint_name(self, tablename, fieldname):
        constraint_name = BaseAdapter.constraint_name(self, tablename, fieldname)
        if len(constraint_name)>30:
            constraint_name = '%s_%s__constraint' % (tablename[:10], fieldname[:7])
        return constraint_name

    def represent_exceptions(self, obj, fieldtype):
        if fieldtype == 'blob':
            obj = base64.b64encode(str(obj))
            return ":CLOB('%s')" % obj
        elif fieldtype == 'date':
            if isinstance(obj, (datetime.date, datetime.datetime)):
                obj = obj.isoformat()[:10]
            else:
                obj = str(obj)
            return "to_date('%s','yyyy-mm-dd')" % obj
        elif fieldtype == 'datetime':
            if isinstance(obj, datetime.datetime):
                obj = obj.isoformat()[:19].replace('T',' ')
            elif isinstance(obj, datetime.date):
                obj = obj.isoformat()[:10]+' 00:00:00'
            else:
                obj = str(obj)
            return "to_date('%s','yyyy-mm-dd hh24:mi:ss')" % obj
        return None

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "oracle"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args,uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.find_or_make_work_folder()
        self.test_query = 'SELECT 1 FROM DUAL;'
        ruri = uri.split('://',1)[1]
        if not 'threaded' in driver_args:
            driver_args['threaded']=True
        def connector(uri=ruri,driver_args=driver_args):
            return self.driver.connect(uri,**driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def after_connection(self):
        self.execute("ALTER SESSION SET NLS_DATE_FORMAT = 'YYYY-MM-DD HH24:MI:SS';")
        self.execute("ALTER SESSION SET NLS_TIMESTAMP_FORMAT = 'YYYY-MM-DD HH24:MI:SS';")

    oracle_fix = re.compile("[^']*('[^']*'[^']*)*\:(?P<clob>CLOB\('([^']+|'')*'\))")

    def execute(self, command, args=None):
        args = args or []
        i = 1
        while True:
            m = self.oracle_fix.match(command)
            if not m:
                break
            command = command[:m.start('clob')] + str(i) + command[m.end('clob'):]
            args.append(m.group('clob')[6:-2].replace("''", "'"))
            i += 1
        if command[-1:]==';':
            command = command[:-1]
        return self.log_execute(command, args)

    def create_sequence_and_triggers(self, query, table, **args):
        tablename = table._rname or table._tablename
        id_name = table._id.name
        sequence_name = table._sequence_name
        trigger_name = table._trigger_name
        self.execute(query)
        self.execute('CREATE SEQUENCE %s START WITH 1 INCREMENT BY 1 NOMAXVALUE MINVALUE -1;' % sequence_name)
        self.execute("""
            CREATE OR REPLACE TRIGGER %(trigger_name)s BEFORE INSERT ON %(tablename)s FOR EACH ROW
            DECLARE
                curr_val NUMBER;
                diff_val NUMBER;
                PRAGMA autonomous_transaction;
            BEGIN
                IF :NEW.%(id)s IS NOT NULL THEN
                    EXECUTE IMMEDIATE 'SELECT %(sequence_name)s.nextval FROM dual' INTO curr_val;
                    diff_val := :NEW.%(id)s - curr_val - 1;
                    IF diff_val != 0 THEN
                      EXECUTE IMMEDIATE 'alter sequence %(sequence_name)s increment by '|| diff_val;
                      EXECUTE IMMEDIATE 'SELECT %(sequence_name)s.nextval FROM dual' INTO curr_val;
                      EXECUTE IMMEDIATE 'alter sequence %(sequence_name)s increment by 1';
                    END IF;
                END IF;
                SELECT %(sequence_name)s.nextval INTO :NEW.%(id)s FROM DUAL;
            END;
        """ % dict(trigger_name=trigger_name, tablename=tablename,
                   sequence_name=sequence_name,id=id_name))

    def lastrowid(self,table):
        sequence_name = table._sequence_name
        self.execute('SELECT %s.currval FROM dual;' % sequence_name)
        return long(self.cursor.fetchone()[0])

    #def parse_value(self, value, field_type, blob_decode=True):
    #    if blob_decode and isinstance(value, cx_Oracle.LOB):
    #        try:
    #            value = value.read()
    #        except self.driver.ProgrammingError:
    #            # After a subsequent fetch the LOB value is not valid anymore
    #            pass
    #    return BaseAdapter.parse_value(self, value, field_type, blob_decode)

    def _fetchall(self):
        if any(x[1]==cx_Oracle.LOB for x in self.cursor.description):
            return [tuple([(c.read() if type(c) == cx_Oracle.LOB else c) \
                               for c in r]) for r in self.cursor]
        else:
            return self.cursor.fetchall()

    def sqlsafe_table(self, tablename, ot=None):
        if ot is not None:
            return (self.QUOTE_TEMPLATE + ' ' \
                    + self.QUOTE_TEMPLATE) % (ot, tablename)
        return self.QUOTE_TEMPLATE % tablename
