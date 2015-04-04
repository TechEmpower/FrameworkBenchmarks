# -*- coding: utf-8 -*-
import re

from .._globals import IDENTITY
from ..drivers import psycopg2_adapt
from ..helpers.methods import varquote_aux
from .base import BaseAdapter
from ..objects import Expression

class PostgreSQLAdapter(BaseAdapter):
    drivers = ('psycopg2','pg8000')

    QUOTE_TEMPLATE = '"%s"'

    support_distributed_transaction = True
    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR(%(length)s)',
        'text': 'TEXT',
        'json': 'TEXT',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'BYTEA',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INTEGER',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT8',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'TIME',
        'datetime': 'TIMESTAMP',
        'id': 'SERIAL PRIMARY KEY',
        'reference': 'INTEGER REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'TEXT',
        'list:string': 'TEXT',
        'list:reference': 'TEXT',
        'geometry': 'GEOMETRY',
        'geography': 'GEOGRAPHY',
        'big-id': 'BIGSERIAL PRIMARY KEY',
        'big-reference': 'BIGINT REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT  "FK_%(constraint_name)s" FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT  "FK_%(foreign_table)s_PK" FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
    }

    def varquote(self,name):
        return varquote_aux(name,'"%s"')

    def adapt(self,obj):
        if self.driver_name == 'psycopg2':
            return psycopg2_adapt(obj).getquoted()
        elif self.driver_name == 'pg8000':
            return "'%s'" % str(obj).replace("%","%%").replace("'","''")
        else:
            return "'%s'" % str(obj).replace("'","''")

    def sequence_name(self,table):
        return self.QUOTE_TEMPLATE % (table + '_id_seq')

    def RANDOM(self):
        return 'RANDOM()'

    def ADD(self, first, second):
        t = first.type
        if t in ('text','string','password', 'json', 'upload','blob'):
            return '(%s || %s)' % (self.expand(first), self.expand(second, t))
        else:
            return '(%s + %s)' % (self.expand(first), self.expand(second, t))

    def distributed_transaction_begin(self,key):
        return

    def prepare(self,key):
        self.execute("PREPARE TRANSACTION '%s';" % key)

    def commit_prepared(self,key):
        self.execute("COMMIT PREPARED '%s';" % key)

    def rollback_prepared(self,key):
        self.execute("ROLLBACK PREPARED '%s';" % key)

    def create_sequence_and_triggers(self, query, table, **args):
        # following lines should only be executed if table._sequence_name does not exist
        # self.execute('CREATE SEQUENCE %s;' % table._sequence_name)
        # self.execute("ALTER TABLE %s ALTER COLUMN %s SET DEFAULT NEXTVAL('%s');" \
        #              % (table._tablename, table._fieldname, table._sequence_name))
        self.execute(query)

    REGEX_URI = re.compile('^(?P<user>[^:@]+)(\:(?P<password>[^@]*))?@(?P<host>[^\:@]+)(\:(?P<port>[0-9]+))?/(?P<db>[^\?]+)(\?sslmode=(?P<sslmode>.+))?$')

    def __init__(self, db,uri, pool_size=0, folder=None, db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, srid=4326,
                 after_connection=None):
        self.db = db
        self.dbengine="postgres"
        self.uri = uri
        if do_connect:
            self.find_driver(adapter_args, uri)
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        self.srid = srid
        self.find_or_make_work_folder()
        self._last_insert = None # for INSERT ... RETURNING ID
        self.TRUE_exp = 'TRUE'
        self.FALSE_exp = 'FALSE'
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
        port = m.group('port') or '5432'
        sslmode = m.group('sslmode')
        driver_args['database'] = db
        driver_args['user'] = user
        driver_args['host'] = host
        driver_args['port'] = int(port)
        driver_args['password'] = password

        if sslmode:
            driver_args['sslmode'] = sslmode

        # choose diver according uri
        if self.driver:
            self.__version__ = "%s %s" % (self.driver.__name__,
                                          self.driver.__version__)
        else:
            self.__version__ = None
        def connector(driver_args=driver_args):
            return self.driver.connect(**driver_args)
        self.connector=connector
        if do_connect:
            self.reconnect()

    def after_connection(self):
        #self.connection.set_client_encoding('UTF8') #pg8000 doesn't have a native set_client_encoding
        self.execute("SET CLIENT_ENCODING TO 'UTF8'")
        self.execute("SET standard_conforming_strings=on;")
        self.try_json()

    def _insert(self, table, fields):
        table_rname = table.sqlsafe
        if fields:
            keys = ','.join(f.sqlsafe_name for f, v in fields)
            values = ','.join(self.expand(v, f.type) for f, v in fields)
            if table._id:
                self._last_insert = (table._id, 1)
                return 'INSERT INTO %s(%s) VALUES (%s) RETURNING %s;' % (
                    table_rname, keys, values, table._id.name)
            else:
                self._last_insert = None
                return 'INSERT INTO %s(%s) VALUES (%s);' % (table_rname, keys, values)
        else:
            self._last_insert
            return self._insert_empty(table)

    def lastrowid(self, table=None):
        if self._last_insert:
            return int(self.cursor.fetchone()[0])
        else:
            self.execute("select lastval()")
            return int(self.cursor.fetchone()[0])

    def try_json(self):
        if self.driver_name == "pg8000":
            supports_json = self.connection._server_version >= "9.2.0"
        elif (self.driver_name == "psycopg2" and
            self.driver.__version__ >= "2.0.12"):
            supports_json = self.connection.server_version >= 90200
        elif self.driver_name == "zxJDBC":
            supports_json = self.connection.dbversion >= "9.2.0"
        else:
            supports_json = None
        if supports_json:
            self.types["json"] = "JSON"
            if ((self.driver_name == "psycopg2" and
                 self.driver.__version__ >= '2.5.0') or
                 (self.driver_name == "pg8000" and
                 self.driver.__version__ >= '1.10.2')):
                self.driver_auto_json = ['loads']
        else:
            self.db.logger.debug("Your database version does not support the JSON"
                " data type (using TEXT instead)")

    def LIKE(self,first,second):
        args = (self.expand(first), self.expand(second,'string'))
        if not first.type in ('string', 'text', 'json'):
            return '(%s LIKE %s)' % (
                self.CAST(args[0], 'CHAR(%s)' % first.length), args[1])
        else:
            return '(%s LIKE %s)' % args

    def ILIKE(self,first,second):
        args = (self.expand(first), self.expand(second,'string'))
        if not first.type in ('string', 'text', 'json', 'list:string'):
            return '(%s LIKE %s)' % (
                self.CAST(args[0], 'CHAR(%s)' % first.length), args[1])
        else:
            return '(%s ILIKE %s)' % args

    def REGEXP(self,first,second):
        return '(%s ~ %s)' % (self.expand(first),
                              self.expand(second,'string'))

    # GIS functions

    def ST_ASGEOJSON(self, first, second):
        """
        http://postgis.org/docs/ST_AsGeoJSON.html
        """
        return 'ST_AsGeoJSON(%s,%s,%s,%s)' %(second['version'],
            self.expand(first), second['precision'], second['options'])

    def ST_ASTEXT(self, first):
        """
        http://postgis.org/docs/ST_AsText.html
        """
        return 'ST_AsText(%s)' %(self.expand(first))

    def ST_X(self, first):
        """
        http://postgis.org/docs/ST_X.html
        """
        return 'ST_X(%s)' %(self.expand(first))

    def ST_Y(self, first):
        """
        http://postgis.org/docs/ST_Y.html
        """
        return 'ST_Y(%s)' %(self.expand(first))

    def ST_CONTAINS(self, first, second):
        """
        http://postgis.org/docs/ST_Contains.html
        """
        return 'ST_Contains(%s,%s)' %(self.expand(first), self.expand(second, first.type))

    def ST_DISTANCE(self, first, second):
        """
        http://postgis.org/docs/ST_Distance.html
        """
        return 'ST_Distance(%s,%s)' %(self.expand(first), self.expand(second, first.type))

    def ST_EQUALS(self, first, second):
        """
        http://postgis.org/docs/ST_Equals.html
        """
        return 'ST_Equals(%s,%s)' %(self.expand(first), self.expand(second, first.type))

    def ST_INTERSECTS(self, first, second):
        """
        http://postgis.org/docs/ST_Intersects.html
        """
        return 'ST_Intersects(%s,%s)' %(self.expand(first), self.expand(second, first.type))

    def ST_OVERLAPS(self, first, second):
        """
        http://postgis.org/docs/ST_Overlaps.html
        """
        return 'ST_Overlaps(%s,%s)' %(self.expand(first), self.expand(second, first.type))

    def ST_SIMPLIFY(self, first, second):
        """
        http://postgis.org/docs/ST_Simplify.html
        """
        return 'ST_Simplify(%s,%s)' %(self.expand(first), self.expand(second, 'double'))

    def ST_SIMPLIFYPRESERVETOPOLOGY(self, first, second):
        """
        http://postgis.org/docs/ST_SimplifyPreserveTopology.html
        """
        return 'ST_SimplifyPreserveTopology(%s,%s)' %(self.expand(first), self.expand(second, 'double'))

    def ST_TOUCHES(self, first, second):
        """
        http://postgis.org/docs/ST_Touches.html
        """
        return 'ST_Touches(%s,%s)' %(self.expand(first), self.expand(second, first.type))

    def ST_WITHIN(self, first, second):
        """
        http://postgis.org/docs/ST_Within.html
        """
        return 'ST_Within(%s,%s)' %(self.expand(first), self.expand(second, first.type))

    def ST_DWITHIN(self, first, (second, third)):
        """
        http://postgis.org/docs/ST_DWithin.html
        """
        return 'ST_DWithin(%s,%s,%s)' %(self.expand(first),
                                        self.expand(second, first.type),
                                        self.expand(third, 'double'))

    def represent(self, obj, fieldtype):
        field_is_type = fieldtype.startswith
        if field_is_type('geo'):
            srid = 4326 # postGIS default srid for geometry
            geotype, parms = fieldtype[:-1].split('(')
            parms = parms.split(',')
            if len(parms) >= 2:
                schema, srid = parms[:2]
            if field_is_type('geometry'):
                value = "ST_GeomFromText('%s',%s)" %(obj, srid)
            elif field_is_type('geography'):
                value = "ST_GeogFromText('SRID=%s;%s')" %(srid, obj)
#             else:
#                 raise SyntaxError('Invalid field type %s' %fieldtype)
            return value
        return BaseAdapter.represent(self, obj, fieldtype)

    def _drop(self, table, mode='restrict'):
        if mode not in ['restrict', 'cascade', '']:
            raise ValueError('Invalid mode: %s' % mode)
        return ['DROP TABLE ' + table.sqlsafe + ' ' + str(mode) + ';']

class NewPostgreSQLAdapter(PostgreSQLAdapter):
    drivers = ('psycopg2','pg8000')

    types = {
        'boolean': 'CHAR(1)',
        'string': 'VARCHAR(%(length)s)',
        'text': 'TEXT',
        'json': 'TEXT',
        'password': 'VARCHAR(%(length)s)',
        'blob': 'BYTEA',
        'upload': 'VARCHAR(%(length)s)',
        'integer': 'INTEGER',
        'bigint': 'BIGINT',
        'float': 'FLOAT',
        'double': 'FLOAT8',
        'decimal': 'NUMERIC(%(precision)s,%(scale)s)',
        'date': 'DATE',
        'time': 'TIME',
        'datetime': 'TIMESTAMP',
        'id': 'SERIAL PRIMARY KEY',
        'reference': 'INTEGER REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'BIGINT[]',
        'list:string': 'TEXT[]',
        'list:reference': 'BIGINT[]',
        'geometry': 'GEOMETRY',
        'geography': 'GEOGRAPHY',
        'big-id': 'BIGSERIAL PRIMARY KEY',
        'big-reference': 'BIGINT REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT  "FK_%(constraint_name)s" FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference TFK': ' CONSTRAINT  "FK_%(foreign_table)s_PK" FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_table)s (%(foreign_key)s) ON DELETE %(on_delete_action)s',
    }

    def parse_list_integers(self, value, field_type):
        return value

    def parse_list_references(self, value, field_type):
        return [self.parse_reference(r, field_type[5:]) for r in value]

    def parse_list_strings(self, value, field_type):
        return value

    def represent(self, obj, fieldtype):
        field_is_type = fieldtype.startswith
        if field_is_type('list:'):
            if not obj:
                obj = []
            elif not isinstance(obj, (list, tuple)):
                obj = [obj]
            if field_is_type('list:string'):
                obj = map(str,obj)
            else:
                obj = map(int,obj)
            return 'ARRAY[%s]' % ','.join(repr(item) for item in obj)
        return PostgreSQLAdapter.represent(self, obj, fieldtype)

    def CONTAINS(self, first, second, case_sensitive=True):
        if first.type.startswith('list'):
            f = self.expand(second, 'string')
            s = self.ANY(first)
            op = self.EQ if case_sensitive == True else self.ILIKE
            return op(f, s)
        else:
            return PostgreSQLAdapter.CONTAINS(self, first, second, case_sensitive=case_sensitive)

    def ANY(self, first):
        return "ANY(%s)" % self.expand(first)

    def ILIKE(self, first, second):
        if first and 'type' not in first:
            args = (first, self.expand(second))
            ilike = '(%s ILIKE %s)' % args
        else:
            ilike = PostgreSQLAdapter.ILIKE(self, first, second)
        return ilike

    def EQ(self, first, second=None):
        if first and 'type' not in first:
            eq = '(%s = %s)' % (first, self.expand(second))
        else:
            eq = PostgreSQLAdapter.EQ(self, first, second)
        return eq


class JDBCPostgreSQLAdapter(PostgreSQLAdapter):
    drivers = ('zxJDBC',)

    REGEX_URI = re.compile('^(?P<user>[^:@]+)(\:(?P<password>[^@]*))?@(?P<host>[^\:/]+)(\:(?P<port>[0-9]+))?/(?P<db>.+)$')

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None    ):
        self.db = db
        self.dbengine = "postgres"
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
        port = m.group('port') or '5432'
        msg = ('jdbc:postgresql://%s:%s/%s' % (host, port, db), user, password)
        def connector(msg=msg,driver_args=driver_args):
            return self.driver.connect(*msg,**driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def after_connection(self):
        self.connection.set_client_encoding('UTF8')
        self.execute('BEGIN;')
        self.execute("SET CLIENT_ENCODING TO 'UNICODE';")
        self.try_json()
