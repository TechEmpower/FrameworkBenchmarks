# -*- coding: utf-8 -*-
import re
import os
import sys
import locale
import datetime
import decimal
import copy
import time
import base64
import types

from .._compat import pjoin, exists, pickle, hashlib_md5, iterkeys
from .._globals import IDENTITY
from .._load import portalocker, json
from .._gae import gae
from ..connection import ConnectionPool
from ..objects import Expression, Field, Query, Table, Row, FieldVirtual, \
    FieldMethod, LazyReferenceGetter, LazySet, VirtualCommand, Rows
from ..helpers.regex import REGEX_NO_GREEDY_ENTITY_NAME, REGEX_TYPE, \
    REGEX_SELECT_AS_PARSER
from ..helpers.methods import xorify, use_common_filters, bar_encode, \
    bar_decode_integer, bar_decode_string
from ..helpers.classes import SQLCustomType, SQLALL, Reference, \
    RecordUpdater, RecordDeleter


TIMINGSSIZE = 100
CALLABLETYPES = (types.LambdaType, types.FunctionType,
                 types.BuiltinFunctionType,
                 types.MethodType, types.BuiltinMethodType)
SELECT_ARGS = set(
    ('orderby', 'groupby', 'limitby', 'required', 'cache', 'left', 'distinct',
     'having', 'join', 'for_update', 'processor', 'cacheable',
     'orderby_on_limitby'))


class AdapterMeta(type):
    """Metaclass to support manipulation of adapter classes.

    At the moment is used to intercept `entity_quoting` argument passed to DAL.
    """

    def __call__(cls, *args, **kwargs):
        uploads_in_blob = kwargs.get('adapter_args', {}).get(
            'uploads_in_blob', cls.uploads_in_blob)
        cls.uploads_in_blob = uploads_in_blob

        entity_quoting = kwargs.get('entity_quoting', False)
        if 'entity_quoting' in kwargs:
            del kwargs['entity_quoting']

        obj = super(AdapterMeta, cls).__call__(*args, **kwargs)
        if not entity_quoting:
            quot = obj.QUOTE_TEMPLATE = '%s'
            regex_ent = r'(\w+)'
        else:
            quot = obj.QUOTE_TEMPLATE
            regex_ent = REGEX_NO_GREEDY_ENTITY_NAME
        obj.REGEX_TABLE_DOT_FIELD = re.compile(
            r'^' + quot % regex_ent + r'\.' + quot % regex_ent + r'$')

        return obj


class BaseAdapter(ConnectionPool):

    __metaclass__ = AdapterMeta

    driver_auto_json = []
    driver = None
    driver_name = None
    drivers = ()  # list of drivers from which to pick
    connection = None
    commit_on_alter_table = False
    support_distributed_transaction = False
    uploads_in_blob = False
    can_select_for_update = True
    dbpath = None
    folder = None
    connector = lambda *args, **kwargs: None  # __init__ should override this
    TRUE_exp = '1'
    FALSE_exp = '0'
    TRUE = 'T'
    FALSE = 'F'
    T_SEP = ' '
    QUOTE_TEMPLATE = '"%s"'
    test_query = 'SELECT 1;'


    types = {
        'boolean': 'CHAR(1)',
        'string': 'CHAR(%(length)s)',
        'text': 'TEXT',
        'json': 'TEXT',
        'password': 'CHAR(%(length)s)',
        'blob': 'BLOB',
        'upload': 'CHAR(%(length)s)',
        'integer': 'INTEGER',
        'bigint': 'INTEGER',
        'float':'DOUBLE',
        'double': 'DOUBLE',
        'decimal': 'DOUBLE',
        'date': 'DATE',
        'time': 'TIME',
        'datetime': 'TIMESTAMP',
        'id': 'INTEGER PRIMARY KEY AUTOINCREMENT',
        'reference': 'INTEGER REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'list:integer': 'TEXT',
        'list:string': 'TEXT',
        'list:reference': 'TEXT',
        # the two below are only used when DAL(...bigint_id=True) and replace 'id','reference'
        'big-id': 'INTEGER PRIMARY KEY AUTOINCREMENT',
        'big-reference': 'INTEGER REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        'reference FK': ', CONSTRAINT  "FK_%(constraint_name)s" FOREIGN KEY (%(field_name)s) REFERENCES %(foreign_key)s ON DELETE %(on_delete_action)s',
        }

    def isOperationalError(self,exception):
        if not hasattr(self.driver, "OperationalError"):
            return None
        return isinstance(exception, self.driver.OperationalError)

    def isProgrammingError(self,exception):
        if not hasattr(self.driver, "ProgrammingError"):
            return None
        return isinstance(exception, self.driver.ProgrammingError)

    def id_query(self, table):
        pkeys = getattr(table,'_primarykey',None)
        if pkeys:
            return table[pkeys[0]] != None
        else:
            return table._id != None

    def adapt(self, obj):
        return "'%s'" % obj.replace("'", "''")

    def smart_adapt(self, obj):
        if isinstance(obj,(int,float)):
            return str(obj)
        return self.adapt(str(obj))

    def file_exists(self, filename):
        #to be used ONLY for files that on GAE may not be on filesystem
        return exists(filename)

    def file_open(self, filename, mode='rb', lock=True):
        #to be used ONLY for files that on GAE may not be on filesystem
        if lock:
            fileobj = portalocker.LockedFile(filename,mode)
        else:
            fileobj = open(filename,mode)
        return fileobj

    def file_close(self, fileobj):
        #to be used ONLY for files that on GAE may not be on filesystem
        if fileobj:
            fileobj.close()

    def file_delete(self, filename):
        os.unlink(filename)

    def find_driver(self, adapter_args, uri=None):
        self.adapter_args = adapter_args
        if getattr(self, 'driver', None) is not None:
            return
        drivers_available = [driver for driver in self.drivers
                             if driver in iterkeys(self.db._drivers_available)]
        if uri:
            items = uri.split('://', 1)[0].split(':')
            request_driver = items[1] if len(items) > 1 else None
        else:
            request_driver = None
        request_driver = request_driver or adapter_args.get('driver')
        if request_driver:
            if request_driver in drivers_available:
                self.driver_name = request_driver
                #self.driver = globals().get(request_driver)
                self.driver = self.db._drivers_available[request_driver]
            else:
                raise RuntimeError("driver %s not available" % request_driver)
        elif drivers_available:
            self.driver_name = drivers_available[0]
            #self.driver = globals().get(self.driver_name)
            self.driver = self.db._drivers_available[self.driver_name]
        else:
            raise RuntimeError("no driver available %s" % str(self.drivers))

    def log(self, message, table=None):
        """ Logs migrations

        It will not log changes if logfile is not specified. Defaults
        to sql.log
        """

        isabs = None
        logfilename = self.adapter_args.get('logfile','sql.log')
        writelog = bool(logfilename)
        if writelog:
            isabs = os.path.isabs(logfilename)

        if table and table._dbt and writelog and self.folder:
            if isabs:
                table._loggername = logfilename
            else:
                table._loggername = pjoin(self.folder, logfilename)
            logfile = self.file_open(table._loggername, 'a')
            logfile.write(message)
            self.file_close(logfile)


    def __init__(self, db,uri,pool_size=0, folder=None, db_codec='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={},do_connect=True, after_connection=None):
        self.db = db
        self.dbengine = "None"
        self.uri = uri
        self.pool_size = pool_size
        self.folder = folder
        self.db_codec = db_codec
        self._after_connection = after_connection
        class Dummy(object):
            lastrowid = 1
            def __getattr__(self, value):
                return lambda *a, **b: []
        self.connection = Dummy()
        self.cursor = Dummy()


    def sequence_name(self,tablename):
        return self.QUOTE_TEMPLATE % ('%s_sequence' % tablename)

    def trigger_name(self,tablename):
        return '%s_sequence' % tablename

    def varquote(self,name):
        return name

    def create_table(self, table,
                     migrate=True,
                     fake_migrate=False,
                     polymodel=None):
        db = table._db
        fields = []
        # PostGIS geo fields are added after the table has been created
        postcreation_fields = []
        sql_fields = {}
        sql_fields_aux = {}
        TFK = {}
        tablename = table._tablename
        sortable = 0
        types = self.types
        for field in table:
            sortable += 1
            field_name = field.name
            field_type = field.type
            if isinstance(field_type,SQLCustomType):
                ftype = field_type.native or field_type.type
            elif field_type.startswith('reference'):
                referenced = field_type[10:].strip()
                if referenced == '.':
                    referenced = tablename
                constraint_name = self.constraint_name(tablename, field_name)
                # if not '.' in referenced \
                #         and referenced != tablename \
                #         and hasattr(table,'_primarykey'):
                #     ftype = types['integer']
                #else:
                try:
                    rtable = db[referenced]
                    rfield = rtable._id
                    rfieldname = rfield.name
                    rtablename = referenced
                except (KeyError, ValueError, AttributeError), e:
                    self.db.logger.debug('Error: %s' % e)
                    try:
                        rtablename,rfieldname = referenced.split('.')
                        rtable = db[rtablename]
                        rfield = rtable[rfieldname]
                    except Exception, e:
                        self.db.logger.debug('Error: %s' %e)
                        raise KeyError('Cannot resolve reference %s in %s definition' % (referenced, table._tablename))

                # must be PK reference or unique
                if getattr(rtable, '_primarykey', None) and rfieldname in rtable._primarykey or \
                        rfield.unique:
                    ftype = types[rfield.type[:9]] % \
                        dict(length=rfield.length)
                    # multicolumn primary key reference?
                    if not rfield.unique and len(rtable._primarykey)>1:
                        # then it has to be a table level FK
                        if rtablename not in TFK:
                            TFK[rtablename] = {}
                        TFK[rtablename][rfieldname] = field_name
                    else:
                        ftype = ftype + \
                            types['reference FK'] % dict(
                                constraint_name = constraint_name, # should be quoted
                                foreign_key = rtable.sqlsafe + ' (' + rfield.sqlsafe_name + ')',
                                table_name = table.sqlsafe,
                                field_name = field.sqlsafe_name,
                                on_delete_action=field.ondelete)
                else:
                    # make a guess here for circular references
                    if referenced in db:
                        id_fieldname = db[referenced]._id.sqlsafe_name
                    elif referenced == tablename:
                        id_fieldname = table._id.sqlsafe_name
                    else: #make a guess
                        id_fieldname = self.QUOTE_TEMPLATE % 'id'
                    #gotcha: the referenced table must be defined before
                    #the referencing one to be able to create the table
                    #Also if it's not recommended, we can still support
                    #references to tablenames without rname to make
                    #migrations and model relationship work also if tables
                    #are not defined in order
                    if referenced == tablename:
                        real_referenced = db[referenced].sqlsafe
                    else:
                        real_referenced = (referenced in db
                                           and db[referenced].sqlsafe
                                           or referenced)
                    rfield = db[referenced]._id
                    ftype = types[field_type[:9]] % dict(
                        index_name = self.QUOTE_TEMPLATE % (field_name+'__idx'),
                        field_name = field.sqlsafe_name,
                        constraint_name = self.QUOTE_TEMPLATE % constraint_name,
                        foreign_key = '%s (%s)' % (real_referenced, rfield.sqlsafe_name),
                        on_delete_action=field.ondelete)
            elif field_type.startswith('list:reference'):
                ftype = types[field_type[:14]]
            elif field_type.startswith('decimal'):
                precision, scale = map(int,field_type[8:-1].split(','))
                ftype = types[field_type[:7]] % \
                    dict(precision=precision,scale=scale)
            elif field_type.startswith('geo'):
                if not hasattr(self,'srid'):
                    raise RuntimeError('Adapter does not support geometry')
                srid = self.srid
                geotype, parms = field_type[:-1].split('(')
                if not geotype in types:
                    raise SyntaxError(
                        'Field: unknown field type: %s for %s' \
                        % (field_type, field_name))
                ftype = types[geotype]
                if self.dbengine == 'postgres' and geotype == 'geometry':
                    if self.ignore_field_case is True:
                        field_name = field_name.lower()
                    # parameters: schema, srid, dimension
                    dimension = 2 # GIS.dimension ???
                    parms = parms.split(',')
                    if len(parms) == 3:
                        schema, srid, dimension = parms
                    elif len(parms) == 2:
                        schema, srid = parms
                    else:
                        schema = parms[0]
                    ftype = "SELECT AddGeometryColumn ('%%(schema)s', '%%(tablename)s', '%%(fieldname)s', %%(srid)s, '%s', %%(dimension)s);" % types[geotype]
                    ftype = ftype % dict(schema=schema,
                                         tablename=tablename,
                                         fieldname=field_name, srid=srid,
                                         dimension=dimension)
                    postcreation_fields.append(ftype)
            elif not field_type in types:
                raise SyntaxError('Field: unknown field type: %s for %s' % \
                    (field_type, field_name))
            else:
                ftype = types[field_type]\
                     % dict(length=field.length)
            if not field_type.startswith('id') and \
                    not field_type.startswith('reference'):
                if field.notnull:
                    ftype += ' NOT NULL'
                else:
                    ftype += self.ALLOW_NULL()
                if field.unique:
                    ftype += ' UNIQUE'
                if field.custom_qualifier:
                    ftype += ' %s' % field.custom_qualifier

            # add to list of fields
            sql_fields[field_name] = dict(
                length=field.length,
                unique=field.unique,
                notnull=field.notnull,
                sortable=sortable,
                type=str(field_type),
                sql=ftype)

            if field.notnull and not field.default is None:
                # Caveat: sql_fields and sql_fields_aux
                # differ for default values.
                # sql_fields is used to trigger migrations and sql_fields_aux
                # is used for create tables.
                # The reason is that we do not want to trigger
                # a migration simply because a default value changes.
                not_null = self.NOT_NULL(field.default, field_type)
                ftype = ftype.replace('NOT NULL', not_null)
            sql_fields_aux[field_name] = dict(sql=ftype)
            # Postgres - PostGIS:
            # geometry fields are added after the table has been created, not now
            if not (self.dbengine == 'postgres' and \
                        field_type.startswith('geom')):
                fields.append('%s %s' % (field.sqlsafe_name, ftype))
        other = ';'

        # backend-specific extensions to fields
        if self.dbengine == 'mysql':
            if not hasattr(table, "_primarykey"):
                fields.append('PRIMARY KEY (%s)' % (self.QUOTE_TEMPLATE % table._id.name))
            engine = self.adapter_args.get('engine','InnoDB')
            other = ' ENGINE=%s CHARACTER SET utf8;' % engine

        fields = ',\n    '.join(fields)
        for rtablename in TFK:
            rfields = TFK[rtablename]
            pkeys = [self.QUOTE_TEMPLATE % pk for pk in db[rtablename]._primarykey]
            fkeys = [self.QUOTE_TEMPLATE % rfields[k].name for k in pkeys ]
            fields = fields + ',\n    ' + \
                types['reference TFK'] % dict(
                table_name = table.sqlsafe,
                field_name=', '.join(fkeys),
                foreign_table = table.sqlsafe,
                foreign_key = ', '.join(pkeys),
                on_delete_action = field.ondelete)

        table_rname = table.sqlsafe

        if getattr(table,'_primarykey',None):
            query = "CREATE TABLE %s(\n    %s,\n    %s) %s" % \
                (table.sqlsafe, fields,
                 self.PRIMARY_KEY(', '.join([self.QUOTE_TEMPLATE % pk for pk in table._primarykey])),other)
        else:
            query = "CREATE TABLE %s(\n    %s\n)%s" % \
                (table.sqlsafe, fields, other)

        if self.uri.startswith('sqlite:///') \
                or self.uri.startswith('spatialite:///'):
            path_encoding = sys.getfilesystemencoding() \
                or locale.getdefaultlocale()[1] or 'utf8'
            dbpath = self.uri[9:self.uri.rfind('/')]\
                .decode('utf8').encode(path_encoding)
        else:
            dbpath = self.folder

        if not migrate:
            return query
        elif self.uri.startswith('sqlite:memory')\
                or self.uri.startswith('spatialite:memory'):
            table._dbt = None
        elif isinstance(migrate, str):
            table._dbt = pjoin(dbpath, migrate)
        else:
            table._dbt = pjoin(
                dbpath, '%s_%s.table' % (table._db._uri_hash, tablename))

        if not table._dbt or not self.file_exists(table._dbt):
            if table._dbt:
                self.log('timestamp: %s\n%s\n'
                         % (datetime.datetime.today().isoformat(),
                            query), table)
            if not fake_migrate:
                self.create_sequence_and_triggers(query,table)
                table._db.commit()
                # Postgres geom fields are added now,
                # after the table has been created
                for query in postcreation_fields:
                    self.execute(query)
                    table._db.commit()
            if table._dbt:
                tfile = self.file_open(table._dbt, 'w')
                pickle.dump(sql_fields, tfile)
                self.file_close(tfile)
                if fake_migrate:
                    self.log('faked!\n', table)
                else:
                    self.log('success!\n', table)
        else:
            tfile = self.file_open(table._dbt, 'r')
            try:
                sql_fields_old = pickle.load(tfile)
            except EOFError:
                self.file_close(tfile)
                raise RuntimeError('File %s appears corrupted' % table._dbt)
            self.file_close(tfile)
            if sql_fields != sql_fields_old:
                self.migrate_table(
                    table,
                    sql_fields, sql_fields_old,
                    sql_fields_aux, None,
                    fake_migrate=fake_migrate
                    )
        return query

    def migrate_table(
        self,
        table,
        sql_fields,
        sql_fields_old,
        sql_fields_aux,
        logfile,
        fake_migrate=False,
        ):

        # logfile is deprecated (moved to adapter.log method)
        db = table._db
        db._migrated.append(table._tablename)
        tablename = table._tablename
        def fix(item):
            k,v=item
            if not isinstance(v,dict):
                v=dict(type='unknown',sql=v)
            if self.ignore_field_case is not True: return k, v
            return k.lower(),v
        # make sure all field names are lower case to avoid
        # migrations because of case cahnge
        sql_fields = dict(map(fix,sql_fields.iteritems()))
        sql_fields_old = dict(map(fix,sql_fields_old.iteritems()))
        sql_fields_aux = dict(map(fix,sql_fields_aux.iteritems()))
        if db._debug:
            db.logger.debug('migrating %s to %s' % (sql_fields_old,sql_fields))

        keys = sql_fields.keys()
        for key in sql_fields_old:
            if not key in keys:
                keys.append(key)
        new_add = self.concat_add(tablename)

        metadata_change = False
        sql_fields_current = copy.copy(sql_fields_old)
        for key in keys:
            query = None
            if not key in sql_fields_old:
                sql_fields_current[key] = sql_fields[key]
                if self.dbengine in ('postgres',) and \
                   sql_fields[key]['type'].startswith('geometry'):
                    # 'sql' == ftype in sql
                    query = [ sql_fields[key]['sql'] ]
                else:
                    query = ['ALTER TABLE %s ADD %s %s;' % \
                         (table.sqlsafe, key,
                          sql_fields_aux[key]['sql'].replace(', ', new_add))]
                metadata_change = True
            elif self.dbengine in ('sqlite', 'spatialite'):
                if key in sql_fields:
                    sql_fields_current[key] = sql_fields[key]
                metadata_change = True
            elif not key in sql_fields:
                del sql_fields_current[key]
                ftype = sql_fields_old[key]['type']
                if (self.dbengine in ('postgres',) and
                    ftype.startswith('geometry')):
                    geotype, parms = ftype[:-1].split('(')
                    schema = parms.split(',')[0]
                    query = [ "SELECT DropGeometryColumn ('%(schema)s', \
                              '%(table)s', '%(field)s');" %
                              dict(schema=schema, table=tablename, field=key) ]
                elif self.dbengine in ('firebird',):
                    query = ['ALTER TABLE %s DROP %s;' %
                             (self.QUOTE_TEMPLATE % tablename, self.QUOTE_TEMPLATE % key)]
                else:
                    query = ['ALTER TABLE %s DROP COLUMN %s;' %
                             (self.QUOTE_TEMPLATE % tablename, self.QUOTE_TEMPLATE % key)]
                metadata_change = True
            elif sql_fields[key]['sql'] != sql_fields_old[key]['sql'] \
                  and not (key in table.fields and
                           isinstance(table[key].type, SQLCustomType)) \
                  and not sql_fields[key]['type'].startswith('reference')\
                  and not sql_fields[key]['type'].startswith('double')\
                  and not sql_fields[key]['type'].startswith('id'):
                sql_fields_current[key] = sql_fields[key]
                t = tablename
                tt = sql_fields_aux[key]['sql'].replace(', ', new_add)
                if self.dbengine in ('firebird',):
                    drop_expr = 'ALTER TABLE %s DROP %s;'
                else:
                    drop_expr = 'ALTER TABLE %s DROP COLUMN %s;'
                key_tmp = key + '__tmp'
                query = ['ALTER TABLE %s ADD %s %s;' % (self.QUOTE_TEMPLATE % t, self.QUOTE_TEMPLATE % key_tmp, tt),
                         'UPDATE %s SET %s=%s;' %
                         (self.QUOTE_TEMPLATE % t, self.QUOTE_TEMPLATE % key_tmp, self.QUOTE_TEMPLATE % key),
                         drop_expr % (self.QUOTE_TEMPLATE % t, self.QUOTE_TEMPLATE % key),
                         'ALTER TABLE %s ADD %s %s;' %
                         (self.QUOTE_TEMPLATE % t, self.QUOTE_TEMPLATE % key, tt),
                         'UPDATE %s SET %s=%s;' %
                         (self.QUOTE_TEMPLATE % t, self.QUOTE_TEMPLATE % key, self.QUOTE_TEMPLATE % key_tmp),
                         drop_expr % (self.QUOTE_TEMPLATE % t, self.QUOTE_TEMPLATE % key_tmp)]
                metadata_change = True
            elif sql_fields[key]['type'] != sql_fields_old[key]['type']:
                sql_fields_current[key] = sql_fields[key]
                metadata_change = True

            if query:
                self.log('timestamp: %s\n'
                    % datetime.datetime.today().isoformat(), table)
                db['_lastsql'] = '\n'.join(query)
                for sub_query in query:
                    self.log(sub_query + '\n', table)
                    if fake_migrate:
                        if db._adapter.commit_on_alter_table:
                            self.save_dbt(table,sql_fields_current)
                        self.log('faked!\n', table)
                    else:
                        self.execute(sub_query)
                        # Caveat: mysql, oracle and firebird
                        # do not allow multiple alter table
                        # in one transaction so we must commit
                        # partial transactions and
                        # update table._dbt after alter table.
                        if db._adapter.commit_on_alter_table:
                            db.commit()
                            self.save_dbt(table,sql_fields_current)
                            self.log('success!\n', table)

            elif metadata_change:
                self.save_dbt(table,sql_fields_current)

        if metadata_change and not (query and db._adapter.commit_on_alter_table):
            db.commit()
            self.save_dbt(table,sql_fields_current)
            self.log('success!\n', table)

    def save_dbt(self,table, sql_fields_current):
        tfile = self.file_open(table._dbt, 'w')
        pickle.dump(sql_fields_current, tfile)
        self.file_close(tfile)

    def LOWER(self, first):
        return 'LOWER(%s)' % self.expand(first)

    def UPPER(self, first):
        return 'UPPER(%s)' % self.expand(first)

    def COUNT(self, first, distinct=None):
        return ('COUNT(%s)' if not distinct else 'COUNT(DISTINCT %s)') \
            % self.expand(first)

    def EXTRACT(self, first, what):
        return "EXTRACT(%s FROM %s)" % (what, self.expand(first))

    def EPOCH(self, first):
        return self.EXTRACT(first, 'epoch')

    def LENGTH(self, first):
        return "LENGTH(%s)" % self.expand(first)

    def AGGREGATE(self, first, what):
        return "%s(%s)" % (what, self.expand(first))

    def JOIN(self):
        return 'JOIN'

    def LEFT_JOIN(self):
        return 'LEFT JOIN'

    def RANDOM(self):
        return 'Random()'

    def NOT_NULL(self, default, field_type):
        return 'NOT NULL DEFAULT %s' % self.represent(default,field_type)

    def COALESCE(self, first, second):
        expressions = [self.expand(first)]+[self.expand(e) for e in second]
        return 'COALESCE(%s)' % ','.join(expressions)

    def COALESCE_ZERO(self, first):
        return 'COALESCE(%s,0)' % self.expand(first)

    def RAW(self, first):
        return first

    def ALLOW_NULL(self):
        return ''

    def SUBSTRING(self, field, parameters):
        return 'SUBSTR(%s,%s,%s)' % (self.expand(field), parameters[0], parameters[1])

    def PRIMARY_KEY(self, key):
        return 'PRIMARY KEY(%s)' % key

    # SQL statement for dropping table
    def _drop(self, table, mode):
        return ['DROP TABLE %s;' % table.sqlsafe]

    # PYDAL cleanup
    def _drop_cleanup(self, table):
        db = table._db
        del db[table._tablename]
        del db.tables[db.tables.index(table._tablename)]
        db._remove_references_to(table)
        if table._dbt:
            self.file_delete(table._dbt)
            self.log('success!\n', table)
        return

    def drop(self, table, mode=''):
        db = table._db
        queries = self._drop(table, mode)
        for query in queries:
            if table._dbt:
                self.log(query + '\n', table)
            self.execute(query)
        db.commit()
        self._drop_cleanup(table)
        return

    def _insert(self, table, fields):
        table_rname = table.sqlsafe
        if fields:
            keys = ','.join(f.sqlsafe_name for f, v in fields)
            values = ','.join(self.expand(v, f.type) for f, v in fields)
            return 'INSERT INTO %s(%s) VALUES (%s);' % (table_rname, keys, values)
        else:
            return self._insert_empty(table)

    def _insert_empty(self, table):
        return 'INSERT INTO %s DEFAULT VALUES;' % (table.sqlsafe)

    def insert(self, table, fields):
        query = self._insert(table,fields)
        try:
            self.execute(query)
        except Exception:
            e = sys.exc_info()[1]
            if hasattr(table,'_on_insert_error'):
                return table._on_insert_error(table,fields,e)
            raise e
        if hasattr(table, '_primarykey'):
            mydict = dict([(k[0].name, k[1]) for k in fields if k[0].name in table._primarykey])
            if mydict != {}:
                return mydict
        id = self.lastrowid(table)
        if hasattr(table, '_primarykey') and len(table._primarykey) == 1:
            id = {table._primarykey[0]: id}
        if not isinstance(id, (int, long)):
            return id
        rid = Reference(id)
        (rid._table, rid._record) = (table, None)
        return rid

    def bulk_insert(self, table, items):
        return [self.insert(table,item) for item in items]

    def NOT(self, first):
        return '(NOT %s)' % self.expand(first)

    def AND(self, first, second):
        return '(%s AND %s)' % (self.expand(first), self.expand(second))

    def OR(self, first, second):
        return '(%s OR %s)' % (self.expand(first), self.expand(second))

    def BELONGS(self, first, second):
        if isinstance(second, str):
            return '(%s IN (%s))' % (self.expand(first), second[:-1])
        if not second:
            return '(1=0)'
        items = ','.join(self.expand(item, first.type) for item in second)
        return '(%s IN (%s))' % (self.expand(first), items)

    def REGEXP(self, first, second):
        """Regular expression operator"""
        raise NotImplementedError

    def LIKE(self, first, second):
        """Case sensitive like operator"""
        return '(%s LIKE %s)' % (self.expand(first),
                                 self.expand(second, 'string'))

    def ILIKE(self, first, second):
        """Case insensitive like operator"""
        return '(LOWER(%s) LIKE %s)' % (self.expand(first),
                                 self.expand(second, 'string').lower())

    def STARTSWITH(self, first, second):
        return '(%s LIKE %s)' % (self.expand(first),
                                 self.expand(second+'%', 'string'))

    def ENDSWITH(self, first, second):
        return '(%s LIKE %s)' % (self.expand(first),
                                 self.expand('%'+second, 'string'))

    def CONTAINS(self, first, second, case_sensitive=True):
        if first.type in ('string','text', 'json'):
            if isinstance(second,Expression):
                second = Expression(None,self.CONCAT('%',Expression(
                            None,self.REPLACE(second,('%','%%'))),'%'))
            else:
                second = '%'+str(second).replace('%','%%')+'%'
        elif first.type.startswith('list:'):
            if isinstance(second,Expression):
                second = Expression(None,self.CONCAT(
                        '%|',Expression(None,self.REPLACE(
                                Expression(None,self.REPLACE(
                                        second,('%','%%'))),('|','||'))),'|%'))
            else:
                second = '%|'+str(second).replace('%','%%')\
                    .replace('|','||')+'|%'
        op = case_sensitive and self.LIKE or self.ILIKE
        return op(first,second)

    def EQ(self, first, second=None):
        if second is None:
            return '(%s IS NULL)' % self.expand(first)
        return '(%s = %s)' % (self.expand(first),
                              self.expand(second, first.type))

    def NE(self, first, second=None):
        if second is None:
            return '(%s IS NOT NULL)' % self.expand(first)
        return '(%s <> %s)' % (self.expand(first),
                               self.expand(second, first.type))

    def LT(self,first,second=None):
        if second is None:
            raise RuntimeError("Cannot compare %s < None" % first)
        return '(%s < %s)' % (self.expand(first),
                              self.expand(second,first.type))

    def LE(self,first,second=None):
        if second is None:
            raise RuntimeError("Cannot compare %s <= None" % first)
        return '(%s <= %s)' % (self.expand(first),
                               self.expand(second,first.type))

    def GT(self,first,second=None):
        if second is None:
            raise RuntimeError("Cannot compare %s > None" % first)
        return '(%s > %s)' % (self.expand(first),
                              self.expand(second,first.type))

    def GE(self,first,second=None):
        if second is None:
            raise RuntimeError("Cannot compare %s >= None" % first)
        return '(%s >= %s)' % (self.expand(first),
                               self.expand(second,first.type))

    def is_numerical_type(self, ftype):
        return ftype in ('integer','boolean','double','bigint') or \
            ftype.startswith('decimal')

    def REPLACE(self, first, (second, third)):
        return 'REPLACE(%s,%s,%s)' % (self.expand(first,'string'),
                                      self.expand(second,'string'),
                                      self.expand(third,'string'))

    def CONCAT(self, *items):
        return '(%s)' % ' || '.join(self.expand(x,'string') for x in items)

    def ADD(self, first, second):
        if self.is_numerical_type(first.type) or isinstance(first.type, Field):
            return '(%s + %s)' % (self.expand(first),
                                  self.expand(second, first.type))
        else:
            return self.CONCAT(first, second)

    def SUB(self, first, second):
        return '(%s - %s)' % (self.expand(first),
                              self.expand(second, first.type))

    def MUL(self, first, second):
        return '(%s * %s)' % (self.expand(first),
                              self.expand(second, first.type))

    def DIV(self, first, second):
        return '(%s / %s)' % (self.expand(first),
                              self.expand(second, first.type))

    def MOD(self, first, second):
        return '(%s %% %s)' % (self.expand(first),
                               self.expand(second, first.type))

    def AS(self, first, second):
        return '%s AS %s'  % (self.expand(first), second)

    def ON(self, first, second):
        table_rname = self.table_alias(first)
        if use_common_filters(second):
            second = self.common_filter(second,[first._tablename])
        return ('%s ON %s') % (self.expand(table_rname), self.expand(second))

    def INVERT(self, first):
        return '%s DESC' % self.expand(first)

    def COMMA(self, first, second):
        return '%s, %s' % (self.expand(first), self.expand(second))

    def CAST(self, first, second):
        return 'CAST(%s AS %s)' % (first, second)

    def expand(self, expression, field_type=None, colnames=False):
        if isinstance(expression, Field):
            et = expression.table
            if not colnames:
                table_rname = et._ot and self.QUOTE_TEMPLATE % et._tablename or et._rname or self.QUOTE_TEMPLATE % et._tablename
                out = '%s.%s' % (table_rname, expression._rname or (self.QUOTE_TEMPLATE % (expression.name)))
            else:
                out = '%s.%s' % (self.QUOTE_TEMPLATE % et._tablename, self.QUOTE_TEMPLATE % expression.name)
            if field_type == 'string' and not expression.type in (
                'string','text','json','password'):
                out = self.CAST(out, self.types['text'])
            return out
        elif isinstance(expression, (Expression, Query)):
            first = expression.first
            second = expression.second
            op = expression.op
            optional_args = expression.optional_args or {}
            if not second is None:
                out = op(first, second, **optional_args)
            elif not first is None:
                out = op(first,**optional_args)
            elif isinstance(op, str):
                if op.endswith(';'):
                    op=op[:-1]
                out = '(%s)' % op
            else:
                out = op()
            return out
        elif field_type:
            return str(self.represent(expression,field_type))
        elif isinstance(expression,(list,tuple)):
            return ','.join(self.represent(item,field_type) \
                                for item in expression)
        elif isinstance(expression, bool):
            return self.db._adapter.TRUE_exp if expression else self.db._adapter.FALSE_exp
        else:
            return str(expression)

    def table_alias(self, tbl):
        if not isinstance(tbl, Table):
            tbl = self.db[tbl]
        return tbl.sqlsafe_alias


    def alias(self, table, alias):
        """
        Given a table object, makes a new table object
        with alias name.
        """
        other = copy.copy(table)
        other['_ot'] = other._ot or other.sqlsafe
        other['ALL'] = SQLALL(other)
        other['_tablename'] = alias
        for fieldname in other.fields:
            other[fieldname] = copy.copy(other[fieldname])
            other[fieldname]._tablename = alias
            other[fieldname].tablename = alias
            other[fieldname].table = other
        table._db[alias] = other
        return other

    def _truncate(self, table, mode=''):
        return ['TRUNCATE TABLE %s %s;' % (table.sqlsafe, mode or '')]

    def truncate(self, table, mode= ' '):
        # Prepare functions "write_to_logfile" and "close_logfile"
        try:
            queries = table._db._adapter._truncate(table, mode)
            for query in queries:
                self.log(query + '\n', table)
                self.execute(query)
            self.log('success!\n', table)
        finally:
            pass

    def _update(self, tablename, query, fields):
        if query:
            if use_common_filters(query):
                query = self.common_filter(query, [tablename])
            sql_w = ' WHERE ' + self.expand(query)
        else:
            sql_w = ''
        sql_v = ','.join(['%s=%s' % (field.sqlsafe_name,
                                     self.expand(value, field.type)) \
                              for (field, value) in fields])
        tablename = self.db[tablename].sqlsafe
        return 'UPDATE %s SET %s%s;' % (tablename, sql_v, sql_w)

    def update(self, tablename, query, fields):
        sql = self._update(tablename, query, fields)
        try:
            self.execute(sql)
        except Exception:
            e = sys.exc_info()[1]
            table = self.db[tablename]
            if hasattr(table,'_on_update_error'):
                return table._on_update_error(table,query,fields,e)
            raise e
        try:
            return self.cursor.rowcount
        except:
            return None

    def _delete(self, tablename, query):
        if query:
            if use_common_filters(query):
                query = self.common_filter(query, [tablename])
            sql_w = ' WHERE ' + self.expand(query)
        else:
            sql_w = ''
        tablename = self.db[tablename].sqlsafe
        return 'DELETE FROM %s%s;' % (tablename, sql_w)

    def delete(self, tablename, query):
        sql = self._delete(tablename, query)
        self.execute(sql)
        try:
            counter = self.cursor.rowcount
        except:
            counter =  None
        return counter

    def get_table(self, query):
        tablenames = self.tables(query)
        if len(tablenames)==1:
            return tablenames[0]
        elif len(tablenames)<1:
            raise RuntimeError("No table selected")
        else:
            raise RuntimeError("Too many tables selected")

    def expand_all(self, fields, tablenames):
        db = self.db
        new_fields = []
        append = new_fields.append
        for item in fields:
            if isinstance(item,SQLALL):
                new_fields += item._table
            elif isinstance(item,str):
                m = self.REGEX_TABLE_DOT_FIELD.match(item)
                if m:
                    tablename,fieldname = m.groups()
                    append(db[tablename][fieldname])
                else:
                    append(Expression(db,lambda item=item:item))
            else:
                append(item)
        # ## if no fields specified take them all from the requested tables
        if not new_fields:
            for table in tablenames:
                for field in db[table]:
                    append(field)
        return new_fields

    def _select(self, query, fields, attributes):
        tables = self.tables
        for key in set(attributes.keys())-SELECT_ARGS:
            raise SyntaxError('invalid select attribute: %s' % key)
        args_get = attributes.get
        tablenames = tables(query)
        tablenames_for_common_filters = tablenames
        for field in fields:
            for tablename in tables(field):
                if not tablename in tablenames:
                    tablenames.append(tablename)

        if len(tablenames) < 1:
            raise SyntaxError('Set: no tables selected')
        def colexpand(field):
            return self.expand(field, colnames=True)
        self._colnames = map(colexpand, fields)
        def geoexpand(field):
            if isinstance(field.type,str) and field.type.startswith('geo') and isinstance(field, Field):
                field = field.st_astext()
            return self.expand(field)
        sql_f = ', '.join(map(geoexpand, fields))
        sql_o = ''
        sql_s = ''
        left = args_get('left', False)
        inner_join = args_get('join', False)
        distinct = args_get('distinct', False)
        groupby = args_get('groupby', False)
        orderby = args_get('orderby', False)
        having = args_get('having', False)
        limitby = args_get('limitby', False)
        orderby_on_limitby = args_get('orderby_on_limitby', True)
        for_update = args_get('for_update', False)
        if self.can_select_for_update is False and for_update is True:
            raise SyntaxError('invalid select attribute: for_update')
        if distinct is True:
            sql_s += 'DISTINCT'
        elif distinct:
            sql_s += 'DISTINCT ON (%s)' % distinct
        if inner_join:
            icommand = self.JOIN()
            if not isinstance(inner_join, (tuple, list)):
                inner_join = [inner_join]
            ijoint = [t._tablename for t in inner_join
                      if not isinstance(t,Expression)]
            ijoinon = [t for t in inner_join if isinstance(t, Expression)]
            itables_to_merge={} #issue 490
            [itables_to_merge.update(
                    dict.fromkeys(tables(t))) for t in ijoinon]
            ijoinont = [t.first._tablename for t in ijoinon]
            [itables_to_merge.pop(t) for t in ijoinont
             if t in itables_to_merge] #issue 490
            iimportant_tablenames = ijoint + ijoinont + itables_to_merge.keys()
            iexcluded = [t for t in tablenames
                         if not t in iimportant_tablenames]
        if left:
            join = attributes['left']
            command = self.LEFT_JOIN()
            if not isinstance(join, (tuple, list)):
                join = [join]
            joint = [t._tablename for t in join
                     if not isinstance(t, Expression)]
            joinon = [t for t in join if isinstance(t, Expression)]
            #patch join+left patch (solves problem with ordering in left joins)
            tables_to_merge={}
            [tables_to_merge.update(
                    dict.fromkeys(tables(t))) for t in joinon]
            joinont = [t.first._tablename for t in joinon]
            [tables_to_merge.pop(t) for t in joinont if t in tables_to_merge]
            tablenames_for_common_filters = [t for t in tablenames
                        if not t in joinont ]
            important_tablenames = joint + joinont + tables_to_merge.keys()
            excluded = [t for t in tablenames
                        if not t in important_tablenames ]
        else:
            excluded = tablenames

        if use_common_filters(query):
            query = self.common_filter(query,tablenames_for_common_filters)
        sql_w = ' WHERE ' + self.expand(query) if query else ''

        JOIN = ' CROSS JOIN '

        if inner_join and not left:
            # Wrap table references with parenthesis (approach 1)
            # sql_t = ', '.join([self.table_alias(t) for t in iexcluded + \
            #                        itables_to_merge.keys()])
            # sql_t = '(%s)' % sql_t
            # or approach 2: Use 'JOIN' instead comma:
            sql_t = JOIN.join([self.table_alias(t)
                               for t in iexcluded + itables_to_merge.keys()])
            for t in ijoinon:
                sql_t += ' %s %s' % (icommand, t)
        elif not inner_join and left:
            sql_t = JOIN.join([self.table_alias(t) for t in excluded + \
                                   tables_to_merge.keys()])
            if joint:
                sql_t += ' %s %s' % (command,
                                     ','.join([t for t in joint]))
            for t in joinon:
                sql_t += ' %s %s' % (command, t)
        elif inner_join and left:
            all_tables_in_query = set(important_tablenames + \
                                      iimportant_tablenames + \
                                      tablenames)
            tables_in_joinon = set(joinont + ijoinont)
            tables_not_in_joinon = \
                all_tables_in_query.difference(tables_in_joinon)
            sql_t = JOIN.join([self.table_alias(t) for t in tables_not_in_joinon])
            for t in ijoinon:
                sql_t += ' %s %s' % (icommand, t)
            if joint:
                sql_t += ' %s %s' % (command,
                                     ','.join([t for t in joint]))
            for t in joinon:
                sql_t += ' %s %s' % (command, t)
        else:
            sql_t = ', '.join(self.table_alias(t) for t in tablenames)
        if groupby:
            if isinstance(groupby, (list, tuple)):
                groupby = xorify(groupby)
            sql_o += ' GROUP BY %s' % self.expand(groupby)
            if having:
                sql_o += ' HAVING %s' % attributes['having']
        if orderby:
            if isinstance(orderby, (list, tuple)):
                orderby = xorify(orderby)
            if str(orderby) == '<random>':
                sql_o += ' ORDER BY %s' % self.RANDOM()
            else:
                sql_o += ' ORDER BY %s' % self.expand(orderby)
        if (limitby and not groupby and tablenames and orderby_on_limitby and not orderby):
            sql_o += ' ORDER BY %s' % ', '.join(
                [self.db[t].sqlsafe + '.' + self.db[t][x].sqlsafe_name for t in tablenames for x in (
                    hasattr(self.db[t], '_primarykey') and self.db[t]._primarykey
                    or ['_id']
                    )
                 ]
                )
        # oracle does not support limitby
        sql = self.select_limitby(sql_s, sql_f, sql_t, sql_w, sql_o, limitby)
        if for_update and self.can_select_for_update is True:
            sql = sql.rstrip(';') + ' FOR UPDATE;'
        return sql

    def select_limitby(self, sql_s, sql_f, sql_t, sql_w, sql_o, limitby):
        if limitby:
            (lmin, lmax) = limitby
            sql_o += ' LIMIT %i OFFSET %i' % (lmax - lmin, lmin)
        return 'SELECT %s %s FROM %s%s%s;' % \
            (sql_s, sql_f, sql_t, sql_w, sql_o)

    def _fetchall(self):
        return self.cursor.fetchall()

    def _select_aux(self,sql,fields,attributes):
        args_get = attributes.get
        cache = args_get('cache',None)
        if not cache:
            self.execute(sql)
            rows = self._fetchall()
        else:
            (cache_model, time_expire) = cache
            key = self.uri + '/' + sql + '/rows'
            key = hashlib_md5(key).hexdigest()
            def _select_aux2():
                self.execute(sql)
                return self._fetchall()
            rows = cache_model(key,_select_aux2,time_expire)
        if isinstance(rows,tuple):
            rows = list(rows)
        limitby = args_get('limitby', None) or (0,)
        rows = self.rowslice(rows,limitby[0],None)
        processor = args_get('processor',self.parse)
        cacheable = args_get('cacheable',False)
        return processor(rows,fields,self._colnames,cacheable=cacheable)

    def select(self, query, fields, attributes):
        """
        Always returns a Rows object, possibly empty.
        """
        sql = self._select(query, fields, attributes)
        cache = attributes.get('cache', None)
        if cache and attributes.get('cacheable',False):
            del attributes['cache']
            (cache_model, time_expire) = cache
            key = self.uri + '/' + sql
            key = hashlib_md5(key).hexdigest()
            args = (sql,fields,attributes)
            return cache_model(
                key,
                lambda self=self,args=args:self._select_aux(*args),
                time_expire)
        else:
            return self._select_aux(sql,fields,attributes)

    def _count(self, query, distinct=None):
        tablenames = self.tables(query)
        if query:
            if use_common_filters(query):
                query = self.common_filter(query, tablenames)
            sql_w = ' WHERE ' + self.expand(query)
        else:
            sql_w = ''
        sql_t = ','.join(self.table_alias(t) for t in tablenames)
        if distinct:
            if isinstance(distinct,(list, tuple)):
                distinct = xorify(distinct)
            sql_d = self.expand(distinct)
            return 'SELECT count(DISTINCT %s) FROM %s%s;' % \
                (sql_d, sql_t, sql_w)
        return 'SELECT count(*) FROM %s%s;' % (sql_t, sql_w)

    def count(self, query, distinct=None):
        self.execute(self._count(query, distinct))
        return self.cursor.fetchone()[0]

    def tables(self, *queries):
        tables = set()
        for query in queries:
            if isinstance(query, Field):
                tables.add(query.tablename)
            elif isinstance(query, (Expression, Query)):
                if not query.first is None:
                    tables = tables.union(self.tables(query.first))
                if not query.second is None:
                    tables = tables.union(self.tables(query.second))
        return list(tables)

    def commit(self):
        if self.connection:
            return self.connection.commit()

    def rollback(self):
        if self.connection:
            return self.connection.rollback()

    def close_connection(self):
        if self.connection:
            r = self.connection.close()
            self.connection = None
            return r

    def distributed_transaction_begin(self, key):
        return

    def prepare(self, key):
        if self.connection: self.connection.prepare()

    def commit_prepared(self, key):
        if self.connection: self.connection.commit()

    def rollback_prepared(self, key):
        if self.connection: self.connection.rollback()

    def concat_add(self, tablename):
        return ', ADD '

    def constraint_name(self, table, fieldname):
        return '%s_%s__constraint' % (table,fieldname)

    def create_sequence_and_triggers(self, query, table, **args):
        self.execute(query)


    def log_execute(self, *a, **b):
        if not self.connection: raise ValueError(a[0])
        if not self.connection: return None
        command = a[0]
        if hasattr(self,'filter_sql_command'):
            command = self.filter_sql_command(command)
        if self.db._debug:
            self.db.logger.debug('SQL: %s' % command)
        self.db._lastsql = command
        t0 = time.time()
        ret = self.cursor.execute(command, *a[1:], **b)
        self.db._timings.append((command,time.time()-t0))
        del self.db._timings[:-TIMINGSSIZE]
        return ret

    def execute(self, *a, **b):
        return self.log_execute(*a, **b)

    def represent(self, obj, fieldtype):
        field_is_type = fieldtype.startswith
        if isinstance(obj, CALLABLETYPES):
            obj = obj()
        if isinstance(fieldtype, SQLCustomType):
            value = fieldtype.encoder(obj)
            if value and fieldtype.type in ('string','text', 'json'):
                return self.adapt(value)
            return value or 'NULL'
        if isinstance(obj, (Expression, Field)):
            return str(obj)
        if field_is_type('list:'):
            if not obj:
                obj = []
            elif not isinstance(obj, (list, tuple)):
                obj = [obj]
            if field_is_type('list:string'):
                obj = map(str,obj)
            else:
                obj = map(int,[o for o in obj if o != ''])
        # we don't want to bar_encode json objects
        if isinstance(obj, (list, tuple)) and (not fieldtype == "json"):
            obj = bar_encode(obj)
        if obj is None:
            return 'NULL'
        if obj == '' and not fieldtype[:2] in ['st', 'te', 'js', 'pa', 'up']:
            return 'NULL'
        r = self.represent_exceptions(obj, fieldtype)
        if not r is None:
            return r
        if fieldtype == 'boolean':
            if obj and not str(obj)[:1].upper() in '0F':
                return self.smart_adapt(self.TRUE)
            else:
                return self.smart_adapt(self.FALSE)
        if fieldtype == 'id' or fieldtype == 'integer':
            return str(long(obj))
        if field_is_type('decimal'):
            return str(obj)
        elif field_is_type('reference'): # reference
            # check for tablename first
            referenced = fieldtype[9:].strip()
            if referenced in self.db.tables:
                return str(long(obj))
            p = referenced.partition('.')
            if p[2] != '':
                try:
                    ftype = self.db[p[0]][p[2]].type
                    return self.represent(obj, ftype)
                except (ValueError, KeyError):
                    return repr(obj)
            elif isinstance(obj, (Row, Reference)):
                return str(obj['id'])
            return str(long(obj))
        elif fieldtype == 'double':
            return repr(float(obj))
        if isinstance(obj, unicode):
            obj = obj.encode(self.db_codec)
        if fieldtype == 'blob':
            obj = base64.b64encode(str(obj))
        elif fieldtype == 'date':
            if isinstance(obj, (datetime.date, datetime.datetime)):
                obj = obj.isoformat()[:10]
            else:
                obj = str(obj)
        elif fieldtype == 'datetime':
            if isinstance(obj, datetime.datetime):
                obj = obj.isoformat(self.T_SEP)[:19]
            elif isinstance(obj, datetime.date):
                obj = obj.isoformat()[:10]+self.T_SEP+'00:00:00'
            else:
                obj = str(obj)
        elif fieldtype == 'time':
            if isinstance(obj, datetime.time):
                obj = obj.isoformat()[:10]
            else:
                obj = str(obj)
        elif fieldtype == 'json':
            if not 'dumps' in self.driver_auto_json:
                # always pass a string JSON string
                if self.db.has_serializer('json'):
                    obj = self.db.serialize('json', obj)
                else:
                    obj = json.dumps(obj)
        if not isinstance(obj, bytes):
            obj = bytes(obj)
        try:
            obj.decode(self.db_codec)
        except:
            obj = obj.decode('latin1').encode(self.db_codec)
        return self.adapt(obj)

    def represent_exceptions(self, obj, fieldtype):
        return None

    def lastrowid(self, table):
        return None

    def rowslice(self, rows, minimum=0, maximum=None):
        """
        By default this function does nothing;
        overload when db does not do slicing.
        """
        return rows

    def parse_value(self, value, field_type, blob_decode=True):
        if field_type != 'blob' and isinstance(value, str):
            try:
                value = value.decode(self.db._db_codec)
            except Exception:
                pass
        if isinstance(value, unicode):
            value = value.encode('utf-8')
        if isinstance(field_type, SQLCustomType):
            value = field_type.decoder(value)
        if not isinstance(field_type, str) or value is None:
            return value
        elif field_type in ('string', 'text', 'password', 'upload', 'dict'):
            return value
        elif field_type.startswith('geo'):
            return value
        elif field_type == 'blob' and not blob_decode:
            return value
        else:
            key = REGEX_TYPE.match(field_type).group(0)
            return self.parsemap[key](value,field_type)

    def parse_reference(self, value, field_type):
        referee = field_type[10:].strip()
        if not '.' in referee:
            value = Reference(value)
            value._table, value._record = self.db[referee], None
        return value

    def parse_boolean(self, value, field_type):
        return value == self.TRUE or str(value)[:1].lower() == 't'

    def parse_date(self, value, field_type):
        if isinstance(value, datetime.datetime):
            return value.date()
        if not isinstance(value, (datetime.date,datetime.datetime)):
            (y, m, d) = map(int, str(value)[:10].strip().split('-'))
            value = datetime.date(y, m, d)
        return value

    def parse_time(self, value, field_type):
        if not isinstance(value, datetime.time):
            time_items = map(int,str(value)[:8].strip().split(':')[:3])
            if len(time_items) == 3:
                (h, mi, s) = time_items
            else:
                (h, mi, s) = time_items + [0]
            value = datetime.time(h, mi, s)
        return value

    def parse_datetime(self, value, field_type):
        if not isinstance(value, datetime.datetime):
            value = str(value)
            date_part,time_part,timezone = value[:10],value[11:19],value[19:]
            if '+' in timezone:
                ms,tz = timezone.split('+')
                h,m = tz.split(':')
                dt = datetime.timedelta(seconds=3600*int(h)+60*int(m))
            elif '-' in timezone:
                ms,tz = timezone.split('-')
                h,m = tz.split(':')
                dt = -datetime.timedelta(seconds=3600*int(h)+60*int(m))
            else:
                dt = None
            (y, m, d) = map(int,date_part.split('-'))
            time_parts = time_part and time_part.split(':')[:3] or (0,0,0)
            while len(time_parts)<3: time_parts.append(0)
            time_items = map(int,time_parts)
            (h, mi, s) = time_items
            value = datetime.datetime(y, m, d, h, mi, s)
            if dt:
                value = value + dt
        return value

    def parse_blob(self, value, field_type):
        return base64.b64decode(str(value))

    def parse_decimal(self, value, field_type):
        decimals = int(field_type[8:-1].split(',')[-1])
        if self.dbengine in ('sqlite', 'spatialite'):
            value = ('%.' + str(decimals) + 'f') % value
        if not isinstance(value, decimal.Decimal):
            value = decimal.Decimal(str(value))
        return value

    def parse_list_integers(self, value, field_type):
        if not isinstance(self, NoSQLAdapter):
            value = bar_decode_integer(value)
        return value

    def parse_list_references(self, value, field_type):
        if not isinstance(self, NoSQLAdapter):
            value = bar_decode_integer(value)
        return [self.parse_reference(r, field_type[5:]) for r in value]

    def parse_list_strings(self, value, field_type):
        if not isinstance(self, NoSQLAdapter):
            value = bar_decode_string(value)
        return value

    def parse_id(self, value, field_type):
        return long(value)

    def parse_integer(self, value, field_type):
        return long(value)

    def parse_double(self, value, field_type):
        return float(value)

    def parse_json(self, value, field_type):
        if not 'loads' in self.driver_auto_json:
            if not isinstance(value, basestring):
                raise RuntimeError('json data not a string')
            if isinstance(value, unicode):
                value = value.encode('utf-8')
            if self.db.has_serializer('loads_json'):
                value = self.db.serialize('loads_json', value)
            else:
                value = json.loads(value)
        return value

    def build_parsemap(self):
        self.parsemap = {
            'id':self.parse_id,
            'integer':self.parse_integer,
            'bigint':self.parse_integer,
            'float':self.parse_double,
            'double':self.parse_double,
            'reference':self.parse_reference,
            'boolean':self.parse_boolean,
            'date':self.parse_date,
            'time':self.parse_time,
            'datetime':self.parse_datetime,
            'blob':self.parse_blob,
            'decimal':self.parse_decimal,
            'json':self.parse_json,
            'list:integer':self.parse_list_integers,
            'list:reference':self.parse_list_references,
            'list:string':self.parse_list_strings,
            }

    def parse(self, rows, fields, colnames, blob_decode=True,
              cacheable = False):
        db = self.db
        virtualtables = []
        new_rows = []
        tmps = []
        for colname in colnames:
            col_m = self.REGEX_TABLE_DOT_FIELD.match(colname)
            if not col_m:
                tmps.append(None)
            else:
                tablename, fieldname = col_m.groups()
                table = db[tablename]
                field = table[fieldname]
                ft = field.type
                tmps.append((tablename, fieldname, table, field, ft))
        for (i,row) in enumerate(rows):
            new_row = Row()
            for (j,colname) in enumerate(colnames):
                value = row[j]
                tmp = tmps[j]
                if tmp:
                    (tablename,fieldname,table,field,ft) = tmp
                    colset = new_row.get(tablename, None)
                    if colset is None:
                        colset = new_row[tablename] = Row()
                        if tablename not in virtualtables:
                            virtualtables.append(tablename)
                    value = self.parse_value(value,ft,blob_decode)
                    if field.filter_out:
                        value = field.filter_out(value)
                    colset[fieldname] = value

                    # for backward compatibility
                    if ft=='id' and fieldname!='id' and \
                            not 'id' in table.fields:
                        colset['id'] = value

                    if ft == 'id' and not cacheable:
                        if self.dbengine == 'google:datastore':
                            id = value.key.id()
                            colset[fieldname] = id
                            colset.gae_item = value
                        else:
                            id = value
                        colset.update_record = RecordUpdater(colset,table,id)
                        colset.delete_record = RecordDeleter(table,id)
                        if table._db._lazy_tables:
                            colset['__get_lazy_reference__'] = LazyReferenceGetter(table, id)
                        for rfield in table._referenced_by:
                            referee_link = db._referee_name and \
                                db._referee_name % dict(
                                table=rfield.tablename,field=rfield.name)
                            if referee_link and not referee_link in colset:
                                colset[referee_link] = LazySet(rfield,id)
                else:
                    if not '_extra' in new_row:
                        new_row['_extra'] = Row()
                    new_row['_extra'][colname] = \
                        self.parse_value(value,
                                         fields[j].type,blob_decode)
                    new_column_name = \
                        REGEX_SELECT_AS_PARSER.search(colname)
                    if not new_column_name is None:
                        column_name = new_column_name.groups(0)
                        setattr(new_row,column_name[0],value)
            new_rows.append(new_row)
        rowsobj = Rows(db, new_rows, colnames, rawrows=rows)


        for tablename in virtualtables:
            table = db[tablename]
            fields_virtual = [(f,v) for (f,v) in table.iteritems()
                              if isinstance(v,FieldVirtual)]
            fields_lazy = [(f,v) for (f,v) in table.iteritems()
                           if isinstance(v,FieldMethod)]
            if fields_virtual or fields_lazy:
                for row in rowsobj.records:
                    box = row[tablename]
                    for f,v in fields_virtual:
                        try:
                            box[f] = v.f(row)
                        except AttributeError:
                            pass # not enough fields to define virtual field
                    for f,v in fields_lazy:
                        try:
                            box[f] = (v.handler or VirtualCommand)(v.f,row)
                        except AttributeError:
                            pass # not enough fields to define virtual field

            ### old style virtual fields
            for item in table.virtualfields:
                try:
                    rowsobj = rowsobj.setvirtualfields(**{tablename:item})
                except (KeyError, AttributeError):
                    # to avoid breaking virtualfields when partial select
                    pass
        return rowsobj

    def common_filter(self, query, tablenames):
        tenant_fieldname = self.db._request_tenant

        for tablename in tablenames:
            table = self.db[tablename]

            # deal with user provided filters
            if table._common_filter != None:
                query = query & table._common_filter(query)

            # deal with multi_tenant filters
            if tenant_fieldname in table:
                default = table[tenant_fieldname].default
                if not default is None:
                    newquery = table[tenant_fieldname] == default
                    if query is None:
                        query = newquery
                    else:
                        query = query & newquery
        return query

    def CASE(self,query,t,f):
        def represent(x):
            types = {type(True):'boolean',type(0):'integer',type(1.0):'double'}
            if x is None: return 'NULL'
            elif isinstance(x,Expression): return str(x)
            else: return self.represent(x,types.get(type(x),'string'))
        return Expression(self.db,'CASE WHEN %s THEN %s ELSE %s END' % \
                              (self.expand(query),represent(t),represent(f)))

    def sqlsafe_table(self, tablename, ot=None):
        if ot is not None:
            return ('%s AS ' + self.QUOTE_TEMPLATE) % (ot, tablename)
        return self.QUOTE_TEMPLATE % tablename

    def sqlsafe_field(self, fieldname):
        return self.QUOTE_TEMPLATE % fieldname


class NoSQLAdapter(BaseAdapter):
    can_select_for_update = False
    QUOTE_TEMPLATE = '%s'

    @staticmethod
    def to_unicode(obj):
        if isinstance(obj, str):
            return obj.decode('utf8')
        elif not isinstance(obj, unicode):
            return unicode(obj)
        return obj

    def id_query(self, table):
        return table._id > 0

    def represent(self, obj, fieldtype):
        field_is_type = fieldtype.startswith
        if isinstance(obj, CALLABLETYPES):
            obj = obj()
        if isinstance(fieldtype, SQLCustomType):
            return fieldtype.encoder(obj)
        is_string = isinstance(fieldtype,str)
        is_list = is_string and field_is_type('list:')
        if is_list:
            if not obj:
                obj = []
            if not isinstance(obj, (list, tuple)):
                obj = [obj]
            obj = [item for item in obj if item]
        if obj == '' and not \
                (is_string and fieldtype[:2] in ['st','te', 'pa','up']):
            return None
        if not obj is None:
            if isinstance(obj, list) and not is_list:
                obj = [self.represent(o, fieldtype) for o in obj]
            elif fieldtype in ('integer','bigint','id'):
                obj = long(obj)
            elif fieldtype == 'double':
                obj = float(obj)
            elif is_string and field_is_type('reference'):
                if isinstance(obj, (Row, Reference)):
                    obj = obj['id']
                obj = long(obj)
            elif fieldtype == 'boolean':
                if obj and not str(obj)[0].upper() in '0F':
                    obj = True
                else:
                    obj = False
            elif fieldtype == 'date':
                if not isinstance(obj, datetime.date):
                    (y, m, d) = map(int,str(obj).strip().split('-'))
                    obj = datetime.date(y, m, d)
                elif isinstance(obj,datetime.datetime):
                    (y, m, d) = (obj.year, obj.month, obj.day)
                    obj = datetime.date(y, m, d)
            elif fieldtype == 'time':
                if not isinstance(obj, datetime.time):
                    time_items = map(int,str(obj).strip().split(':')[:3])
                    if len(time_items) == 3:
                        (h, mi, s) = time_items
                    else:
                        (h, mi, s) = time_items + [0]
                    obj = datetime.time(h, mi, s)
            elif fieldtype == 'datetime':
                if not isinstance(obj, datetime.datetime):
                    (y, m, d) = map(int,str(obj)[:10].strip().split('-'))
                    time_items = map(int,str(obj)[11:].strip().split(':')[:3])
                    while len(time_items)<3:
                        time_items.append(0)
                    (h, mi, s) = time_items
                    obj = datetime.datetime(y, m, d, h, mi, s)
            elif fieldtype == 'blob':
                pass
            elif fieldtype == 'json':
                if isinstance(obj, basestring):
                    obj = self.to_unicode(obj)
                    if self.db.has_serializer('loads_json'):
                        obj = self.db.serialize('loads_json', obj)
                    else:
                        obj = json.loads(obj)
            elif is_string and field_is_type('list:string'):
                return map(self.to_unicode,obj)
            elif is_list:
                return map(int,obj)
            else:
                obj = self.to_unicode(obj)
        return obj

    def _insert(self,table,fields):
        return 'insert %s in %s' % (fields, table)

    def _count(self,query,distinct=None):
        return 'count %s' % repr(query)

    def _select(self,query,fields,attributes):
        return 'select %s where %s' % (repr(fields), repr(query))

    def _delete(self,tablename, query):
        return 'delete %s where %s' % (repr(tablename),repr(query))

    def _update(self,tablename,query,fields):
        return 'update %s (%s) where %s' % (repr(tablename),
                                            repr(fields),repr(query))

    def commit(self):
        """
        remember: no transactions on many NoSQL
        """
        pass

    def rollback(self):
        """
        remember: no transactions on many NoSQL
        """
        pass

    def close_connection(self):
        """
        remember: no transactions on many NoSQL
        """
        pass


    # these functions should never be called!
    def OR(self,first,second): raise SyntaxError("Not supported")
    def AND(self,first,second): raise SyntaxError("Not supported")
    def AS(self,first,second): raise SyntaxError("Not supported")
    def ON(self,first,second): raise SyntaxError("Not supported")
    def STARTSWITH(self,first,second=None): raise SyntaxError("Not supported")
    def ENDSWITH(self,first,second=None): raise SyntaxError("Not supported")
    def ADD(self,first,second): raise SyntaxError("Not supported")
    def SUB(self,first,second): raise SyntaxError("Not supported")
    def MUL(self,first,second): raise SyntaxError("Not supported")
    def DIV(self,first,second): raise SyntaxError("Not supported")
    def LOWER(self,first): raise SyntaxError("Not supported")
    def UPPER(self,first): raise SyntaxError("Not supported")
    def EXTRACT(self,first,what): raise SyntaxError("Not supported")
    def LENGTH(self, first): raise SyntaxError("Not supported")
    def AGGREGATE(self,first,what): raise SyntaxError("Not supported")
    def LEFT_JOIN(self): raise SyntaxError("Not supported")
    def RANDOM(self): raise SyntaxError("Not supported")
    def SUBSTRING(self,field,parameters):  raise SyntaxError("Not supported")
    def PRIMARY_KEY(self,key):  raise SyntaxError("Not supported")
    def ILIKE(self,first,second): raise SyntaxError("Not supported")
    def drop(self,table,mode):  raise SyntaxError("Not supported")
    def migrate_table(self,*a,**b): raise SyntaxError("Not supported")
    def distributed_transaction_begin(self,key): raise SyntaxError("Not supported")
    def prepare(self,key): raise SyntaxError("Not supported")
    def commit_prepared(self,key): raise SyntaxError("Not supported")
    def rollback_prepared(self,key): raise SyntaxError("Not supported")
    def concat_add(self,table): raise SyntaxError("Not supported")
    def constraint_name(self, table, fieldname): raise SyntaxError("Not supported")
    def create_sequence_and_triggers(self, query, table, **args): pass
    def log_execute(self,*a,**b): raise SyntaxError("Not supported")
    def execute(self,*a,**b): raise SyntaxError("Not supported")
    def represent_exceptions(self, obj, fieldtype): raise SyntaxError("Not supported")
    def lastrowid(self,table): raise SyntaxError("Not supported")
    def rowslice(self,rows,minimum=0,maximum=None): raise SyntaxError("Not supported")
