# -*- coding: utf-8 -*-
import re
import sys

from .._globals import IDENTITY
from ..helpers.methods import varquote_aux
from .base import BaseAdapter


class MSSQLAdapter(BaseAdapter):
    drivers = ('pyodbc',)
    T_SEP = 'T'

    QUOTE_TEMPLATE = '"%s"'

    types = {
        'boolean': 'BIT',
        'string': 'VARCHAR(%(length)s)',
        'text': 'TEXT',
        'json': 'TEXT',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'IMAGE',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATETIME',
        'time': 'CHAR(8)',
        'datetime': 'DATETIME',
        'id': 'INT IDENTITY PRIMARY KEY',
        'reference': 'INT NULL, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'TEXT',
        'list:string': 'TEXT',
        'list:reference': 'TEXT',
        'geometry': 'geometry',
        'geography': 'geography',
        'big-id': 'BIGINT IDENTITY PRIMARY KEY',
        'big-reference': 'BIGINT NULL, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
        }

    def concat_add(self,tablename):
        return '; ALTER TABLE %s ADD ' % tablename

    def varquote(self,name):
        return varquote_aux(name,'[%s]')

    def EXTRACT(self,field,what):
        return "DATEPART(%s,%s)" % (what, self.expand(field))

    def LEFT_JOIN(self):
        return 'LEFT OUTER JOIN'

    def RANDOM(self):
        return 'NEWID()'

    def ALLOW_NULL(self):
        return ' NULL'

    def CAST(self, first, second):
        return first # apparently no cast necessary in MSSQL

    def SUBSTRING(self,field,parameters):
        return 'SUBSTRING(%s,%s,%s)' % (self.expand(field), parameters[0], parameters[1])

    def PRIMARY_KEY(self,key):
        return 'PRIMARY KEY CLUSTERED (%s)' % key

    def AGGREGATE(self, first, what):
        if what == 'LENGTH':
            what = 'LEN'
        return "%s(%s)" % (what, self.expand(first))

    def LENGTH(self, first):
        return "LEN(%s)" % self.expand(first)

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            sql_s += ' TOP %i' % lmax
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)

    TRUE = 1
    FALSE = 0

    REGEX_DSN = re.compile('^(?P<dsn>.+)$')
    REGEX_URI = re.compile('^(?P<user>[^:@]+)(\:(?P<password>[^@]*))?@(?P<host>[^\:/]+)(\:(?P<port>[0-9]+))?/(?P<db>[^\?]+)(\?(?P<urlargs>.*))?$')
    REGEX_ARGPATTERN = re.compile('(?P<argkey>[^=]+)=(?P<argvalue>[^&]*)')

    def __init__(self, db, uri, pool_size=0, folder=None, db_codec='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, srid=4326,
                 after_connection=None):
        self.db = db
        self.dbengine = "mssql"
        self.uri = uri
        if do_connect: self.find_driver(adapter_args,uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.srid = srid
        self.find_or_make_work_folder()
        # ## read: http://bytes.com/groups/python/460325-cx_oracle-utf8
        ruri = uri.split('://', 1)[1]
        if '@' not in ruri:
            try:
                m = self.REGEX_DSN.match(ruri)
                if not m:
                    raise SyntaxError(
                        'Parsing uri string(%s) has no result' % self.uri)
                dsn = m.group('dsn')
                if not dsn:
                    raise SyntaxError('DSN required')
            except SyntaxError:
                e = sys.exc_info()[1]
                self.db.logger.error('NdGpatch error')
                raise e
            # was cnxn = 'DSN=%s' % dsn
            cnxn = dsn
        else:
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
            port = m.group('port') or '1433'
            # Parse the optional url name-value arg pairs after the '?'
            # (in the form of arg1=value1&arg2=value2&...)
            # Default values (drivers like FreeTDS insist on uppercase parameter keys)
            argsdict = { 'DRIVER':'{SQL Server}' }
            urlargs = m.group('urlargs') or ''
            for argmatch in self.REGEX_ARGPATTERN.finditer(urlargs):
                argsdict[str(argmatch.group('argkey')).upper()] = argmatch.group('argvalue')
            urlargs = ';'.join(['%s=%s' % (ak, av) for (ak, av) in argsdict.iteritems()])
            cnxn = 'SERVER=%s;PORT=%s;DATABASE=%s;UID=%s;PWD=%s;%s' \
                % (host, port, db, user, password, urlargs)
        def connector(cnxn=cnxn,driver_args=driver_args):
            return self.driver.connect(cnxn, **driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def lastrowid(self, table):
        #self.execute('SELECT @@IDENTITY;')
        self.execute('SELECT SCOPE_IDENTITY();')
        return long(self.cursor.fetchone()[0])

    def rowslice(self, rows, minimum=0, maximum=None):
        if maximum is None:
            return rows[minimum:]
        return rows[minimum:maximum]

    def EPOCH(self, first):
        return "DATEDIFF(second, '1970-01-01 00:00:00', %s)" % self.expand(first)

    def CONCAT(self, *items):
        return '(%s)' % ' + '.join(self.expand(x,'string') for x in items)

    # GIS Spatial Extensions

    # No STAsGeoJSON in MSSQL

    def ST_ASTEXT(self, first):
        return '%s.STAsText()' %(self.expand(first))

    def ST_CONTAINS(self, first, second):
        return '%s.STContains(%s)=1' %(self.expand(first), self.expand(second, first.type))

    def ST_DISTANCE(self, first, second):
        return '%s.STDistance(%s)' %(self.expand(first), self.expand(second, first.type))

    def ST_EQUALS(self, first, second):
        return '%s.STEquals(%s)=1' %(self.expand(first), self.expand(second, first.type))

    def ST_INTERSECTS(self, first, second):
        return '%s.STIntersects(%s)=1' %(self.expand(first), self.expand(second, first.type))

    def ST_OVERLAPS(self, first, second):
        return '%s.STOverlaps(%s)=1' %(self.expand(first), self.expand(second, first.type))

    # no STSimplify in MSSQL

    def ST_TOUCHES(self, first, second):
        return '%s.STTouches(%s)=1' %(self.expand(first), self.expand(second, first.type))

    def ST_WITHIN(self, first, second):
        return '%s.STWithin(%s)=1' %(self.expand(first), self.expand(second, first.type))

    def represent(self, obj, fieldtype):
        field_is_type = fieldtype.startswith
        if field_is_type('geometry'):
            srid = 0 # MS SQL default srid for geometry
            geotype, parms = fieldtype[:-1].split('(')
            if parms:
                srid = parms
            return "geometry::STGeomFromText('%s',%s)" %(obj, srid)
        elif fieldtype == 'geography':
            srid = 4326 # MS SQL default srid for geography
            geotype, parms = fieldtype[:-1].split('(')
            if parms:
                srid = parms
            return "geography::STGeomFromText('%s',%s)" %(obj, srid)
#             else:
#                 raise SyntaxError('Invalid field type %s' %fieldtype)
            return "geometry::STGeomFromText('%s',%s)" %(obj, srid)
        return BaseAdapter.represent(self, obj, fieldtype)


class MSSQL3Adapter(MSSQLAdapter):
    """Experimental support for pagination in MSSQL

    Requires MSSQL >= 2005, uses `ROW_NUMBER()`
    """

    types = {
        'boolean': 'BIT',
        'string': 'VARCHAR(%(length)s)',
        'text': 'VARCHAR(MAX)',
        'json': 'VARCHAR(MAX)',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'IMAGE',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATETIME',
        'time': 'TIME(7)',
        'datetime': 'DATETIME',
        'id': 'INT IDENTITY PRIMARY KEY',
        'reference': 'INT NULL, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'VARCHAR(MAX)',
        'list:string': 'VARCHAR(MAX)',
        'list:reference': 'VARCHAR(MAX)',
        'geometry': 'geometry',
        'geography': 'geography',
        'big-id': 'BIGINT IDENTITY PRIMARY KEY',
        'big-reference': 'BIGINT NULL, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
    }

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            if lmin == 0:
                sql_s += ' TOP %i' % lmax
                return 'SELECT %s %s FROM %s%s%s;' % (sql_s, sql_f, sql_t, sql_w, sql_o)
            lmin += 1
            sql_o_inner = sql_o[sql_o.find('ORDER BY ')+9:]
            sql_g_inner = sql_o[:sql_o.find('ORDER BY ')]
            sql_f_outer = ['f_%s' % f for f in range(len(sql_f.split(',')))]
            sql_f_inner = [f for f in sql_f.split(',')]
            sql_f_iproxy = ['%s AS %s' % (o, n) for (o, n) in zip(sql_f_inner, sql_f_outer)]
            sql_f_iproxy = ', '.join(sql_f_iproxy)
            sql_f_oproxy = ', '.join(sql_f_outer)
            return 'SELECT %s %s FROM (SELECT %s ROW_NUMBER() OVER (ORDER BY %s) AS w_row, %s FROM %s%s%s) TMP WHERE w_row BETWEEN %i AND %s;' % (sql_s,sql_f_oproxy,sql_s,sql_f,sql_f_iproxy,sql_t,sql_w,sql_g_inner,lmin,lmax)
        return 'SELECT %s %s FROM %s%s%s;' % (sql_s,sql_f,sql_t,sql_w,sql_o)
    def rowslice(self,rows,minimum=0,maximum=None):
        return rows


class MSSQL4Adapter(MSSQLAdapter):
    """Support for "native" pagination

    Requires MSSQL >= 2012, uses `OFFSET ... ROWS ... FETCH NEXT ... ROWS ONLY`
    """

    types = {
        'boolean': 'BIT',
        'string': 'VARCHAR(%(length)s)',
        'text': 'VARCHAR(MAX)',
        'json': 'VARCHAR(MAX)',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'IMAGE',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATETIME',
        'time': 'TIME(7)',
        'datetime': 'DATETIME',
        'id': 'INT IDENTITY PRIMARY KEY',
        'reference': 'INT NULL, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'VARCHAR(MAX)',
        'list:string': 'VARCHAR(MAX)',
        'list:reference': 'VARCHAR(MAX)',
        'geometry': 'geometry',
        'geography': 'geography',
        'big-id': 'BIGINT IDENTITY PRIMARY KEY',
        'big-reference': 'BIGINT NULL, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
    }

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            if lmin == 0:
                #top is still slightly faster, especially because
                #web2py's default to fetch references is to not specify
                #an orderby clause
                sql_s += ' TOP %i' % lmax
            else:
                if not sql_o:
                    #if there is no orderby, we can't use the brand new statements
                    #that being said, developer chose its own poison, so be it random
                    sql_o += ' ORDER BY %s' % self.RANDOM()
                sql_o += ' OFFSET %i ROWS FETCH NEXT %i ROWS ONLY' % (lmin, lmax - lmin)
        return 'SELECT %s %s FROM %s%s%s;' % \
                (sql_s, sql_f, sql_t, sql_w, sql_o)

    def rowslice(self, rows, minimum=0, maximum=None):
        return rows


class MSSQL2Adapter(MSSQLAdapter):
    drivers = ('pyodbc',)

    types = {
        'boolean': 'CHAR(1)',
        'string': 'NVARCHAR(%(length)s)',
        'text': 'NTEXT',
        'json': 'NTEXT',
        'password': 'NVARCHAR(%(length)s)',
        'blob': 'IMAGE',
        'upload': 'NVARCHAR(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATETIME',
        'time': 'CHAR(8)',
        'datetime': 'DATETIME',
        'id': 'INT IDENTITY PRIMARY KEY',
        'reference': 'INT, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'NTEXT',
        'list:string': 'NTEXT',
        'list:reference': 'NTEXT',
        'big-id': 'BIGINT IDENTITY PRIMARY KEY',
        'big-reference': 'BIGINT, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
        }

    def represent(self, obj, fieldtype):
        value = BaseAdapter.represent(self, obj, fieldtype)
        if fieldtype in ('string', 'text', 'json') and value[:1] == "'":
            value = 'N' + value
        return value

    def execute(self, a):
        return self.log_execute(a.decode('utf8'))


class VerticaAdapter(MSSQLAdapter):
    drivers = ('pyodbc',)
    T_SEP = ' '

    types = {
        'boolean': 'BOOLEAN',
        'string': 'VARCHAR(%(length)s)',
        'text': 'BYTEA',
        'json': 'VARCHAR(%(length)s)',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'BYTEA',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'DOUBLE PRECISION',
        'decimal': 'DECIMAL(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'TIME',
        'datetime': 'DATETIME',
        'id': 'IDENTITY',
        'reference': 'INT REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'BYTEA',
        'list:string': 'BYTEA',
        'list:reference': 'BYTEA',
        'big-reference': 'BIGINT REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        }


    def EXTRACT(self, first, what):
        return "DATE_PART('%s', TIMESTAMP %s)" % (what, self.expand(first))

    def _truncate(self, table, mode=''):
        tablename = table._tablename
        return ['TRUNCATE %s %s;' % (tablename, mode or '')]

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            sql_o += ' LIMIT %i OFFSET %i' % (lmax - lmin, lmin)
        return 'SELECT %s %s FROM %s%s%s;' % \
            (sql_s, sql_f, sql_t, sql_w, sql_o)

    def lastrowid(self, table):
        self.execute('SELECT LAST_INSERT_ID();')
        return long(self.cursor.fetchone()[0])

    def execute(self, a):
        return self.log_execute(a)


class SybaseAdapter(MSSQLAdapter):
    drivers = ('Sybase')

    types = {
        'boolean': 'BIT',
        'string': 'CHAR VARYING(%(length)s)',
        'text': 'TEXT',
        'json': 'TEXT',
        'password': 'CHAR VARYING(%(length)s)',
        'blob': 'IMAGE',
        'upload': 'CHAR VARYING(%(length)s)',
        'integer': 'INT',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATETIME',
        'time': 'CHAR(8)',
        'datetime': 'DATETIME',
        'id': 'INT IDENTITY PRIMARY KEY',
        'reference': 'INT NULL, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'TEXT',
        'list:string': 'TEXT',
        'list:reference': 'TEXT',
        'geometry': 'geometry',
        'geography': 'geography',
        'big-id': 'BIGINT IDENTITY PRIMARY KEY',
        'big-reference': 'BIGINT NULL, CONSTRAINT %(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT FK_%(constraint_name)s FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT FK_%(foreign_table)s_PK FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
    }

    def __init__(self, db, uri, pool_size=0, folder=None, db_codec='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, srid=4326,
                 after_connection=None):
        self.db = db
        self.dbengine = "sybase"
        self.uri = uri
        if do_connect:
            self.find_driver(adapter_args, uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.srid = srid
        self.find_or_make_work_folder()
        # ## read: http://bytes.com/groups/python/460325-cx_oracle-utf8
        ruri = uri.split('://', 1)[1]
        if '@' not in ruri:
            try:
                m = self.REGEX_DSN.match(ruri)
                if not m:
                    raise SyntaxError(
                        'Parsing uri string(%s) has no result' % self.uri)
                dsn = m.group('dsn')
                if not dsn:
                    raise SyntaxError('DSN required')
            except SyntaxError:
                e = sys.exc_info()[1]
                self.db.logger.error('NdGpatch error')
                raise e
        else:
            m = self.REGEX_URI.match(uri)
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
            port = m.group('port') or '1433'

            dsn = 'sybase:host=%s:%s;dbname=%s' % (host, port, db)

            driver_args.update(user=credential_decoder(user),
                               passwd=credential_decoder(password))

        def connector(dsn=dsn, driver_args=driver_args):
            return self.driver.connect(dsn, **driver_args)
        self.connector = connector
        if do_connect:
            self.reconnect()
