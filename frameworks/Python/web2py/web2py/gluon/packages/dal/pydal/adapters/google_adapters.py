# -*- coding: utf-8 -*-
import os
import re

from .._compat import pjoin
from .._globals import IDENTITY, THREAD_LOCAL
from .._load import json
from .._gae import classobj, gae, ndb, namespace_manager, NDBPolyModel, rdbms
from ..objects import Table, Field, Expression, Query
from ..helpers.classes import SQLCustomType, SQLALL, \
    Reference, UseDatabaseStoredFile
from ..helpers.methods import use_common_filters, xorify
from ..helpers.gae import NDBDecimalProperty
from .base import NoSQLAdapter
from .mysql import MySQLAdapter


class GoogleSQLAdapter(UseDatabaseStoredFile, MySQLAdapter):
    uploads_in_blob = True

    REGEX_URI = re.compile('^(?P<instance>.*)/(?P<db>.*)$')

    def clear_cache(self):
        ndb.get_context().clear_cache()

    def ignore_cache_for(self, entities = None):
        entities = entities or []
        ndb.get_context().set_cache_policy(lambda key: key.kind() not in entities)

    def __init__(self, db, uri='google:sql://realm:domain/database',
                 pool_size=0, folder=None, db_codec='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):

        self.db = db
        self.dbengine = "mysql"
        self.uri = uri
        self.pool_size = pool_size
        self.db_codec = db_codec
        self._after_connection = after_connection
        if do_connect: self.find_driver(adapter_args, uri)
        self.folder = folder or pjoin('$HOME',THREAD_LOCAL.folder.split(
                os.sep+'applications'+os.sep,1)[1])
        ruri = uri.split("://")[1]
        m = self.REGEX_URI.match(ruri)
        if not m:
            raise SyntaxError("Invalid URI string in SQLDB: %s" % self.uri)
        instance = credential_decoder(m.group('instance'))
        self.dbstring = db = credential_decoder(m.group('db'))
        driver_args['instance'] = instance
        if not 'charset' in driver_args:
            driver_args['charset'] = 'utf8'
        self.createdb = createdb = adapter_args.get('createdb',True)
        if not createdb:
            driver_args['database'] = db
        def connector(driver_args=driver_args):
            return rdbms.connect(**driver_args)
        self.connector = connector
        if do_connect: self.reconnect()

    def after_connection(self):
        if self.createdb:
            # self.execute('DROP DATABASE %s' % self.dbstring)
            self.execute('CREATE DATABASE IF NOT EXISTS %s' % self.dbstring)
            self.execute('USE %s' % self.dbstring)
        self.execute("SET FOREIGN_KEY_CHECKS=1;")
        self.execute("SET sql_mode='NO_BACKSLASH_ESCAPES';")

    def execute(self, command, *a, **b):
        return self.log_execute(command.decode('utf8'), *a, **b)

    def find_driver(self,adapter_args,uri=None):
        self.adapter_args = adapter_args
        self.driver = "google"

class GoogleDatastoreAdapter(NoSQLAdapter):
    """
    This now always uses NDB since there is no reason to use DB:

    You can enable NDB as follows:

        db = DAL('google:datastore')

    You can also pass optional ndb_settings:

        db = DAL('google:datastore',
                 adapter_args = {'ndb_settings': ndb_settings})

    ndb_settings is optional and can be used for per model caching settings.
    ndb_settings must be a dict in this form::

        ndb_settings = {<table_name>:{<variable_name>:<variable_value>}}

    See: https://developers.google.com/appengine/docs/python/ndb/cache
    """

    MAX_FETCH_LIMIT = 1000000
    uploads_in_blob = True
    types = {}
    # reconnect is not required for Datastore dbs
    reconnect = lambda *args, **kwargs: None

    def file_exists(self, filename): pass
    def file_open(self, filename, mode='rb', lock=True): pass
    def file_close(self, fileobj): pass

    REGEX_NAMESPACE = re.compile('.*://(?P<namespace>.+)')

    def __init__(self,db,uri,pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.types.update({
                'boolean': ndb.BooleanProperty,
                'string': (lambda **kwargs: ndb.StringProperty(**kwargs)),
                'text': ndb.TextProperty,
                'json': ndb.TextProperty,
                'password': ndb.StringProperty,
                'blob': ndb.BlobProperty,
                'upload': ndb.StringProperty,
                'integer': ndb.IntegerProperty,
                'bigint': ndb.IntegerProperty,
                'float': ndb.FloatProperty,
                'double': ndb.FloatProperty,
                'decimal': NDBDecimalProperty,
                'date': ndb.DateProperty,
                'time': ndb.TimeProperty,
                'datetime': ndb.DateTimeProperty,
                'id': None,
                'reference': ndb.IntegerProperty,
                'list:string': (lambda **kwargs: ndb.StringProperty(repeated=True,default=None, **kwargs)),
                'list:integer': (lambda **kwargs: ndb.IntegerProperty(repeated=True,default=None, **kwargs)),
                'list:reference': (lambda **kwargs: ndb.IntegerProperty(repeated=True,default=None, **kwargs)),
                })
        self.db = db
        self.uri = uri
        self.dbengine = 'google:datastore'
        self.folder = folder
        db['_lastsql'] = ''
        self.db_codec = 'UTF-8'
        self._after_connection = after_connection
        self.pool_size = 0
        match = self.REGEX_NAMESPACE.match(uri)
        if match:
            namespace_manager.set_namespace(match.group('namespace'))

        self.ndb_settings = adapter_args.get('ndb_settings')

    def parse_id(self, value, field_type):
        return value

    def represent(self, obj, fieldtype, tablename=None):
        if isinstance(obj, ndb.Key):
            return obj
        elif fieldtype == 'id' and tablename:
            if isinstance(obj, list):
                return [self.represent(item,fieldtype,tablename) for item in obj]
            elif obj is None:
                return None
            else:
                return ndb.Key(tablename, long(obj))
        elif fieldtype == "json":
            if self.db.has_serializer('json'):
                return self.db.serialize('json', obj)
            else:
                return json.dumps(obj)
        elif isinstance(obj, (Expression, Field)):
            raise SyntaxError("non supported on GAE")
        elif isinstance(fieldtype, gae.Property):
            return obj
        elif fieldtype.startswith('list:') and not isinstance(obj, list):
            if fieldtype=='list:string': return str(obj)
            else: return long(obj)
        else:
            obj = NoSQLAdapter.represent(self, obj, fieldtype)
            return obj

    def create_table(self,table,migrate=True,fake_migrate=False, polymodel=None):
        myfields = {}
        for field in table:
            if isinstance(polymodel,Table) and field.name in polymodel.fields():
                continue
            attr = {}
            if isinstance(field.custom_qualifier, dict):
                #this is custom properties to add to the GAE field declartion
                attr = field.custom_qualifier
            field_type = field.type
            if isinstance(field_type, SQLCustomType):
                ftype = self.types[field_type.native or field_type.type](**attr)
            elif isinstance(field_type, ndb.Property):
                ftype = field_type
            elif field_type.startswith('id'):
                continue
            elif field_type.startswith('decimal'):
                precision, scale = field_type[7:].strip('()').split(',')
                precision = int(precision)
                scale = int(scale)
                dec_cls = NDBDecimalProperty
                ftype = dec_cls(precision, scale, **attr)
            elif field_type.startswith('reference'):
                if field.notnull:
                    attr = dict(required=True)
                ftype = self.types[field_type[:9]](**attr)
            elif field_type.startswith('list:reference'):
                if field.notnull:
                    attr['required'] = True
                ftype = self.types[field_type[:14]](**attr)
            elif field_type.startswith('list:'):
                ftype = self.types[field_type](**attr)
            elif not field_type in self.types\
                 or not self.types[field_type]:
                raise SyntaxError('Field: unknown field type: %s' % field_type)
            else:
                ftype = self.types[field_type](**attr)
            myfields[field.name] = ftype
        if not polymodel:
            model_cls = ndb.Model
            table._tableobj =  classobj(table._tablename, (model_cls, ), myfields)
            # Set NDB caching variables
            if self.ndb_settings and (table._tablename in self.ndb_settings):
                for k, v in self.ndb_settings.iteritems():
                    setattr(table._tableobj, k, v)
        elif polymodel==True:
            pm_cls = NDBPolyModel
            table._tableobj = classobj(table._tablename, (pm_cls, ), myfields)
        elif isinstance(polymodel,Table):
            table._tableobj = classobj(table._tablename, (polymodel._tableobj, ), myfields)
        else:
            raise SyntaxError("polymodel must be None, True, a table or a tablename")
        return None

    def expand(self,expression,field_type=None):
        if expression is None:
            return None
        elif isinstance(expression,Field):
            if expression.type in ('text', 'blob', 'json'):
                raise SyntaxError('AppEngine does not index by: %s' % expression.type)
            return expression.name
        elif isinstance(expression, (Expression, Query)):
            if not expression.second is None:
                return expression.op(expression.first, expression.second)
            elif not expression.first is None:
                return expression.op(expression.first)
            else:
                return expression.op()
        elif field_type:
                return self.represent(expression,field_type)
        elif isinstance(expression,(list,tuple)):
            return ','.join([self.represent(item,field_type) for item in expression])
        elif hasattr(expression, "_FilterNode__name"):
            # check for _FilterNode__name to avoid explicit import of FilterNode
            return expression
        else:
            raise NotImplementedError

    def AND(self,first,second):
        first = self.expand(first)
        second = self.expand(second)
        # none means lack of query (true)
        if first == None: return second
        return ndb.AND(first, second)

    def OR(self,first,second):
        first = self.expand(first)
        second = self.expand(second)
        # none means lack of query (true)
        if first == None or second == None: return None
        return ndb.OR(first, second)

    GAE_FILTER_OPTIONS = {
        '=': lambda a,b: a==b,
        '>': lambda a,b: a>b,
        '<': lambda a,b: a<b,
        '<=': lambda a,b: a<=b,
        '>=': lambda a,b: a>=b,
        '!=': lambda a,b: a!=b,
        'in': lambda a,b: a.IN(b),
        }

    def gaef(self,first, op, second):
        name = first.name if first.name != 'id' else 'key'
        if name == 'key' and op in ('>','!=') and second in (0,'0', None):
            return  None
        field = getattr(first.table._tableobj, name)
        value = self.represent(second,first.type,first._tablename)
        token = self.GAE_FILTER_OPTIONS[op](field,value)
        return token

    def EQ(self,first,second=None):
        return self.gaef(first,'=',second)

    def NE(self,first,second=None):
        return self.gaef(first,'!=',second)

    def LT(self,first,second=None):
        return self.gaef(first,'<',second)

    def LE(self,first,second=None):
        return self.gaef(first,'<=',second)

    def GT(self,first,second=None):
        return self.gaef(first,'>',second)

    def GE(self,first,second=None):
        return self.gaef(first,'>=',second)

    def INVERT(self,first):
        return '-%s' % first.name

    def COMMA(self,first,second):
        return '%s, %s' % (first,second)

    def BELONGS(self,first,second=None):
        if not isinstance(second,(list, tuple, set)):
            raise SyntaxError("Not supported")
        if not isinstance(second, list):
            second = list(second)
        return self.gaef(first,'in',second)

    def CONTAINS(self,first,second,case_sensitive=False):
        # silently ignoring: GAE can only do case sensitive matches!
        if not first.type.startswith('list:'):
            raise SyntaxError("Not supported")
        return self.gaef(first,'=',second)

    def NOT(self, first):
        op, f, s = first.op, first.first, first.second
        if op in [self.OR, self.AND]:
            not_op = self.AND if op == self.OR else self.OR
            r = not_op(self.NOT(f), self.NOT(s))
        elif op == self.EQ:
            r = self.gaef(f, '!=', s)
        elif op == self.NE:
            r = self.gaef(f, '==', s)
        elif op == self.LT:
            r = self.gaef(f, '>=', s)
        elif op == self.LE:
            r = self.gaef(f, '>', s)
        elif op == self.GT:
            r = self.gaef(f, '<=', s)
        elif op == self.GE:
            r = self.gaef(f, '<', s)
        else:
            # TODO the IN operator must be split into a sequence of
            # (field!=value) AND (field!=value) AND ...
            raise NotImplementedError
        return r

    def truncate(self,table,mode):
        self.db(self.db._adapter.id_query(table)).delete()

    def select_raw(self,query,fields=None,attributes=None,count_only=False):
        db = self.db
        fields = fields or []
        attributes = attributes or {}
        args_get = attributes.get
        new_fields = []

        for item in fields:
            if isinstance(item,SQLALL):
                new_fields += item._table
            else:
                new_fields.append(item)

        fields = new_fields
        if query:
            tablename = self.get_table(query)
        elif fields:
            tablename = fields[0].tablename
            query = db._adapter.id_query(fields[0].table)
        else:
            raise SyntaxError("Unable to determine a tablename")

        if query:
            if use_common_filters(query):
                query = self.common_filter(query,[tablename])

        #tableobj is a GAE/NDB Model class (or subclass)
        tableobj = db[tablename]._tableobj
        filters = self.expand(query)

        ## DETERMINE PROJECTION
        projection = None
        if len(db[tablename].fields) == len(fields):
            # getting all fields, not a projection query
            projection = None
        elif args_get('projection') == True:
            projection = []
            for f in fields:
                if f.type in ['text', 'blob', 'json']:
                    raise SyntaxError(
                        "text and blob field types not allowed in projection queries")
                else:
                    projection.append(f.name)

        elif args_get('filterfields') is True:
            projection = []
            for f in fields:
                projection.append(f.name)

        # real projection's can't include 'id'.
        # it will be added to the result later
        if projection and args_get('projection') == True:
            query_projection = filter(lambda p: p != db[tablename]._id.name,
                                      projection)
        else:
            query_projection = None
        ## DONE WITH PROJECTION

        cursor = args_get('reusecursor')
        cursor = cursor if isinstance(cursor, str) else None
        qo = ndb.QueryOptions(projection=query_projection, cursor=cursor)

        if filters == None:
            items = tableobj.query(default_options=qo)
        elif (hasattr(filters,'_FilterNode__name') and
            filters._FilterNode__name=='__key__' and
            filters._FilterNode__opsymbol=='='):
            item = ndb.Key.from_old_key(filters._FilterNode__value).get()
            items = [item] if item else []
        else:
            items = tableobj.query(filters, default_options=qo)

        if count_only:
            items = [len(items) if isinstance(items,list) else items.count()]
        elif not isinstance(items,list):
            if args_get('left', None):
                raise SyntaxError('Set: no left join in appengine')
            if args_get('groupby', None):
                raise SyntaxError('Set: no groupby in appengine')
            orderby = args_get('orderby', False)
            if orderby:
                if isinstance(orderby, (list, tuple)):
                    orderby = xorify(orderby)
                if isinstance(orderby,Expression):
                    orderby = self.expand(orderby)
                orders = orderby.split(', ')
                tbl = tableobj
                for order in orders:
                    order = str(order)
                    desc = order[:1] == '-'
                    name = order[1 if desc else 0:].split('.')[-1]
                    if name == 'id':
                        o = -tbl._key if desc else tbl._key
                    else:
                        o = -getattr(tbl, name) if desc else getattr(tbl, name)
                    items = items.order(o)

            if args_get('limitby', None):
                (lmin, lmax) = attributes['limitby']
                limit, fetch_args = lmax-lmin, {'offset':lmin,'keys_only':True}

                keys, cursor, more = items.fetch_page(limit,**fetch_args)
                items = ndb.get_multi(keys)
                # cursor is only useful if there was a limit and we
                # didn't return all results
                if args_get('reusecursor'):
                    db['_lastcursor'] = cursor

        return (items, tablename, projection or db[tablename].fields)

    def select(self,query,fields,attributes):
        """
        This is the GAE version of select. Some notes to consider:
            - db['_lastsql'] is not set because there is not SQL statement string
                for a GAE query
            - 'nativeRef' is a magical fieldname used for self references on GAE
            - optional attribute 'projection' when set to True will trigger
                use of the GAE projection queries.  note that there are rules for
                what is accepted imposed by GAE: each field must be indexed,
                projection queries cannot contain blob or text fields, and you
                cannot use == and also select that same field.
                see https://developers.google.com/appengine/docs/python/datastore/queries#Query_Projection
            - optional attribute 'filterfields' when set to True web2py will only
                parse the explicitly listed fields into the Rows object, even though
                all fields are returned in the query.  This can be used to reduce
                memory usage in cases where true projection queries are not
                usable.
            - optional attribute 'reusecursor' allows use of cursor with queries
                that have the limitby attribute.  Set the attribute to True for the
                first query, set it to the value of db['_lastcursor'] to continue
                a previous query.  The user must save the cursor value between
                requests, and the filters must be identical.  It is up to the user
                to follow google's limitations:
                https://developers.google.com/appengine/docs/python/datastore/queries#Query_Cursors
        """

        (items, tablename, fields) = self.select_raw(query,fields,attributes)
        # self.db['_lastsql'] = self._select(query,fields,attributes)
        rows = [[(t==self.db[tablename]._id.name and item) or \
                 (t=='nativeRef' and item) or getattr(item, t) \
                     for t in fields] for item in items]
        colnames = ['%s.%s' % (tablename, t) for t in fields]
        processor = attributes.get('processor',self.parse)
        return processor(rows,fields,colnames,False)

    def parse_list_integers(self, value, field_type):
        return value[:]

    def parse_list_strings(self, value, field_type):
        return value[:]

    def count(self,query,distinct=None,limit=None):
        if distinct:
            raise RuntimeError("COUNT DISTINCT not supported")
        (items, tablename, fields) = self.select_raw(query,count_only=True)
        return items[0]

    def delete(self,tablename, query):
        """
        This function was changed on 2010-05-04 because according to
        http://code.google.com/p/googleappengine/issues/detail?id=3119
        GAE no longer supports deleting more than 1000 records.
        """
        # self.db['_lastsql'] = self._delete(tablename,query)
        (items, tablename, fields) = self.select_raw(query)
        # items can be one item or a query
        if not isinstance(items,list):
            # use a keys_only query to ensure that this runs as a datastore
            # small operations
            leftitems = items.fetch(1000, keys_only=True)
            counter = 0
            while len(leftitems):
                counter += len(leftitems)
                ndb.delete_multi(leftitems)
                leftitems = items.fetch(1000, keys_only=True)
        else:
            counter = len(items)
            ndb.delete_multi([item.key for item in items])

        return counter

    def update(self,tablename,query,update_fields):
        # self.db['_lastsql'] = self._update(tablename,query,update_fields)
        (items, tablename, fields) = self.select_raw(query)
        counter = 0
        for item in items:
            for field, value in update_fields:
                setattr(item, field.name, self.represent(value,field.type))
            item.put()
            counter += 1
        self.db.logger.info(str(counter))
        return counter

    def insert(self,table,fields):
        dfields=dict((f.name,self.represent(v,f.type)) for f,v in fields)
        # table._db['_lastsql'] = self._insert(table,fields)
        tmp = table._tableobj(**dfields)
        tmp.put()
        key = tmp.key
        rid = Reference(key.id())
        (rid._table, rid._record, rid._gaekey) = (table, None, key)
        return rid

    def bulk_insert(self,table,items):
        parsed_items = []
        for item in items:
            dfields=dict((f.name,self.represent(v,f.type)) for f,v in item)
            parsed_items.append(table._tableobj(**dfields))
        return ndb.put_multi(parsed_items)
