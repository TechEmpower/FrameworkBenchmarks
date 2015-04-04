# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
|

This file contains the DAL support for many relational databases, including:

  - SQLite & SpatiaLite
  - MySQL
  - Postgres
  - Firebird
  - Oracle
  - MS SQL
  - DB2
  - Interbase
  - Ingres
  - Informix (9+ and SE)
  - SapDB (experimental)
  - Cubrid (experimental)
  - CouchDB (experimental)
  - MongoDB (in progress)
  - Google:nosql
  - Google:sql
  - Teradata
  - IMAP (experimental)

Example of usage::

    >>> # from dal import DAL, Field

    ### create DAL connection (and create DB if it doesn't exist)
    >>> db = DAL(('sqlite://storage.sqlite','mysql://a:b@localhost/x'),
    ... folder=None)

    ### define a table 'person' (create/alter as necessary)
    >>> person = db.define_table('person',Field('name','string'))

    ### insert a record
    >>> id = person.insert(name='James')

    ### retrieve it by id
    >>> james = person(id)

    ### retrieve it by name
    >>> james = person(name='James')

    ### retrieve it by arbitrary query
    >>> query = (person.name=='James') & (person.name.startswith('J'))
    >>> james = db(query).select(person.ALL)[0]

    ### update one record
    >>> james.update_record(name='Jim')
    <Row {'id': 1, 'name': 'Jim'}>

    ### update multiple records by query
    >>> db(person.name.like('J%')).update(name='James')
    1

    ### delete records by query
    >>> db(person.name.lower() == 'jim').delete()
    0

    ### retrieve multiple records (rows)
    >>> people = db(person).select(orderby=person.name,
    ... groupby=person.name, limitby=(0,100))

    ### further filter them
    >>> james = people.find(lambda row: row.name == 'James').first()
    >>> print james.id, james.name
    1 James

    ### check aggregates
    >>> counter = person.id.count()
    >>> print db(person).select(counter).first()(counter)
    1

    ### delete one record
    >>> james.delete_record()
    1

    ### delete (drop) entire database table
    >>> person.drop()


Supported DAL URI strings::

    'sqlite://test.db'
    'spatialite://test.db'
    'sqlite:memory'
    'spatialite:memory'
    'jdbc:sqlite://test.db'
    'mysql://root:none@localhost/test'
    'postgres://mdipierro:password@localhost/test'
    'postgres:psycopg2://mdipierro:password@localhost/test'
    'postgres:pg8000://mdipierro:password@localhost/test'
    'jdbc:postgres://mdipierro:none@localhost/test'
    'mssql://web2py:none@A64X2/web2py_test'
    'mssql2://web2py:none@A64X2/web2py_test' # alternate mappings
    'mssql3://web2py:none@A64X2/web2py_test' # better pagination (requires >= 2005)
    'mssql4://web2py:none@A64X2/web2py_test' # best pagination (requires >= 2012)
    'oracle://username:password@database'
    'firebird://user:password@server:3050/database'
    'db2:ibm_db_dbi://DSN=dsn;UID=user;PWD=pass'
    'db2:pyodbc://driver=DB2;hostname=host;database=database;uid=user;pwd=password;port=port'
    'firebird://username:password@hostname/database'
    'firebird_embedded://username:password@c://path'
    'informix://user:password@server:3050/database'
    'informixu://user:password@server:3050/database' # unicode informix
    'ingres://database'  # or use an ODBC connection string, e.g. 'ingres://dsn=dsn_name'
    'google:datastore' # for google app engine datastore (uses ndb by default)
    'google:sql' # for google app engine with sql (mysql compatible)
    'teradata://DSN=dsn;UID=user;PWD=pass; DATABASE=database' # experimental
    'imap://user:password@server:port' # experimental
    'mongodb://user:password@server:port/database' # experimental

For more info::

    help(DAL)
    help(Field)

"""

import threading
import socket
import urllib
import time
import copy
import traceback
import glob
import logging
from uuid import uuid4

from ._compat import pickle, hashlib_md5, pjoin, ogetattr, osetattr, copyreg
from ._globals import GLOBAL_LOCKER, THREAD_LOCAL, DEFAULT, GLOBALS
from ._load import OrderedDict
from .helpers.classes import SQLCallableList
from .helpers.methods import hide_password, smart_query, auto_validators, \
    auto_represent
from .helpers.regex import REGEX_PYTHON_KEYWORDS, REGEX_DBNAME, REGEX_SEARCH_PATTERN, REGEX_SQUARE_BRACKETS
from .objects import Table, Field, Row, Set
from .adapters import ADAPTERS
from .adapters.base import BaseAdapter


TABLE_ARGS = set(
    ('migrate','primarykey','fake_migrate','format','redefine',
     'singular','plural','trigger_name','sequence_name','fields',
     'common_filter','polymodel','table_class','on_define','rname'))


class MetaDAL(type):
    def __call__(cls, *args, **kwargs):
        #: intercept arguments for DAL costumisation on call
        intercepts = [
            'logger', 'representers', 'serializers', 'uuid', 'validators',
            'validators_method']
        intercepted = []
        for name in intercepts:
            val = kwargs.get(name)
            if val:
                intercepted.append((name, val))
                del kwargs[name]
        for tup in intercepted:
            setattr(cls, tup[0], tup[1])

        obj = super(MetaDAL, cls).__call__(*args, **kwargs)
        return obj


class DAL(object):

    """
    An instance of this class represents a database connection

    Args:
        uri(str): contains information for connecting to a database.
            Defaults to `'sqlite://dummy.db'`

            Note:
                experimental: you can specify a dictionary as uri
                parameter i.e. with::

                    db = DAL({"uri": "sqlite://storage.sqlite",
                              "tables": {...}, ...})

                for an example of dict input you can check the output
                of the scaffolding db model with

                    db.as_dict()

                Note that for compatibility with Python older than
                version 2.6.5 you should cast your dict input keys
                to str due to a syntax limitation on kwarg names.
                for proper DAL dictionary input you can use one of::

                    obj = serializers.cast_keys(dict, [encoding="utf-8"])
                    #or else (for parsing json input)
                    obj = serializers.loads_json(data, unicode_keys=False)

        pool_size: How many open connections to make to the database object.
        folder: where .table files will be created. Automatically set within
            web2py. Use an explicit path when using DAL outside web2py
        db_codec: string encoding of the database (default: 'UTF-8')
        table_hash: database identifier with .tables. If your connection hash
                    change you can still using old .tables if they have db_hash
                    as prefix
        check_reserved: list of adapters to check tablenames and column names
            against sql/nosql reserved keywords. Defaults to `None`

            - 'common' List of sql keywords that are common to all database
              types such as "SELECT, INSERT". (recommended)
            - 'all' Checks against all known SQL keywords
            - '<adaptername>'' Checks against the specific adapters list of
              keywords
            - '<adaptername>_nonreserved' Checks against the specific adapters
              list of nonreserved keywords. (if available)

        migrate: sets default migrate behavior for all tables
        fake_migrate: sets default fake_migrate behavior for all tables
        migrate_enabled: If set to False disables ALL migrations
        fake_migrate_all: If set to True fake migrates ALL tables
        attempts: Number of times to attempt connecting
        auto_import: If set to True, tries import automatically table
            definitions from the databases folder (works only for simple models)
        bigint_id: If set, turn on bigint instead of int for id and reference
            fields
        lazy_tables: delaya table definition until table access
        after_connection: can a callable that will be executed after the
            connection

    Example:
        Use as::

           db = DAL('sqlite://test.db')

        or::

           db = DAL(**{"uri": ..., "tables": [...]...}) # experimental

           db.define_table('tablename', Field('fieldname1'),
                                        Field('fieldname2'))


    """
    __metaclass__ = MetaDAL

    serializers = None
    validators = None
    validators_method = None
    representers = {}
    uuid = lambda x: str(uuid4())
    logger = logging.getLogger("pyDAL")

    Table = Table

    def __new__(cls, uri='sqlite://dummy.db', *args, **kwargs):
        if not hasattr(THREAD_LOCAL, 'db_instances'):
            THREAD_LOCAL.db_instances = {}
        if not hasattr(THREAD_LOCAL, 'db_instances_zombie'):
            THREAD_LOCAL.db_instances_zombie = {}
        if uri == '<zombie>':
            db_uid = kwargs['db_uid']  # a zombie must have a db_uid!
            if db_uid in THREAD_LOCAL.db_instances:
                db_group = THREAD_LOCAL.db_instances[db_uid]
                db = db_group[-1]
            elif db_uid in THREAD_LOCAL.db_instances_zombie:
                db = THREAD_LOCAL.db_instances_zombie[db_uid]
            else:
                db = super(DAL, cls).__new__(cls)
                THREAD_LOCAL.db_instances_zombie[db_uid] = db
        else:
            db_uid = kwargs.get('db_uid', hashlib_md5(repr(uri)).hexdigest())
            if db_uid in THREAD_LOCAL.db_instances_zombie:
                db = THREAD_LOCAL.db_instances_zombie[db_uid]
                del THREAD_LOCAL.db_instances_zombie[db_uid]
            else:
                db = super(DAL, cls).__new__(cls)
            db_group = THREAD_LOCAL.db_instances.get(db_uid, [])
            db_group.append(db)
            THREAD_LOCAL.db_instances[db_uid] = db_group
        db._db_uid = db_uid
        return db

    @staticmethod
    def set_folder(folder):
        # ## this allows gluon to set a folder for this thread
        # ## <<<<<<<<< Should go away as new DAL replaces old sql.py
        BaseAdapter.set_folder(folder)

    @staticmethod
    def get_instances():
        """
        Returns a dictionary with uri as key with timings and defined tables::

            {'sqlite://storage.sqlite': {
                'dbstats': [(select auth_user.email from auth_user, 0.02009)],
                'dbtables': {
                    'defined': ['auth_cas', 'auth_event', 'auth_group',
                        'auth_membership', 'auth_permission', 'auth_user'],
                    'lazy': '[]'
                    }
                }
            }

        """
        dbs = getattr(THREAD_LOCAL, 'db_instances', {}).items()
        infos = {}
        for db_uid, db_group in dbs:
            for db in db_group:
                if not db._uri:
                    continue
                k = hide_password(db._adapter.uri)
                infos[k] = dict(
                    dbstats = [(row[0], row[1]) for row in db._timings],
                    dbtables = {'defined': sorted(
                            list(set(db.tables)-set(db._LAZY_TABLES.keys()))),
                                'lazy': sorted(db._LAZY_TABLES.keys())})
        return infos

    @staticmethod
    def distributed_transaction_begin(*instances):
        if not instances:
            return
        thread_key = '%s.%s' % (socket.gethostname(), threading.currentThread())
        keys = ['%s.%i' % (thread_key, i) for (i,db) in instances]
        instances = enumerate(instances)
        for (i, db) in instances:
            if not db._adapter.support_distributed_transaction():
                raise SyntaxError(
                    'distributed transaction not suported by %s' % db._dbname)
        for (i, db) in instances:
            db._adapter.distributed_transaction_begin(keys[i])

    @staticmethod
    def distributed_transaction_commit(*instances):
        if not instances:
            return
        instances = enumerate(instances)
        thread_key = '%s.%s' % (socket.gethostname(), threading.currentThread())
        keys = ['%s.%i' % (thread_key, i) for (i,db) in instances]
        for (i, db) in instances:
            if not db._adapter.support_distributed_transaction():
                raise SyntaxError(
                    'distributed transaction not suported by %s' % db._dbanme)
        try:
            for (i, db) in instances:
                db._adapter.prepare(keys[i])
        except:
            for (i, db) in instances:
                db._adapter.rollback_prepared(keys[i])
            raise RuntimeError('failure to commit distributed transaction')
        else:
            for (i, db) in instances:
                db._adapter.commit_prepared(keys[i])
        return

    def __init__(self, uri='sqlite://dummy.db',
                 pool_size=0, folder=None,
                 db_codec='UTF-8', check_reserved=None,
                 migrate=True, fake_migrate=False,
                 migrate_enabled=True, fake_migrate_all=False,
                 decode_credentials=False, driver_args=None,
                 adapter_args=None, attempts=5, auto_import=False,
                 bigint_id=False, debug=False, lazy_tables=False,
                 db_uid=None, do_connect=True,
                 after_connection=None, tables=None, ignore_field_case=True,
                 entity_quoting=False, table_hash=None):

        if uri == '<zombie>' and db_uid is not None:
            return

        from .drivers import DRIVERS, is_jdbc
        self._drivers_available = DRIVERS

        if not decode_credentials:
            credential_decoder = lambda cred: cred
        else:
            credential_decoder = lambda cred: urllib.unquote(cred)
        self._folder = folder
        if folder:
            self.set_folder(folder)
        self._uri = uri
        self._pool_size = pool_size
        self._db_codec = db_codec
        self._lastsql = ''
        self._timings = []
        self._pending_references = {}
        self._request_tenant = 'request_tenant'
        self._common_fields = []
        self._referee_name = '%(table)s'
        self._bigint_id = bigint_id
        self._debug = debug
        self._migrated = []
        self._LAZY_TABLES = {}
        self._lazy_tables = lazy_tables
        self._tables = SQLCallableList()
        self._driver_args = driver_args
        self._adapter_args = adapter_args
        self._check_reserved = check_reserved
        self._decode_credentials = decode_credentials
        self._attempts = attempts
        self._do_connect = do_connect
        self._ignore_field_case = ignore_field_case

        if not str(attempts).isdigit() or attempts < 0:
            attempts = 5
        if uri:
            uris = isinstance(uri, (list, tuple)) and uri or [uri]
            error = ''
            connected = False
            for k in range(attempts):
                for uri in uris:
                    try:
                        if is_jdbc and not uri.startswith('jdbc:'):
                            uri = 'jdbc:'+uri
                        self._dbname = REGEX_DBNAME.match(uri).group()
                        if not self._dbname in ADAPTERS:
                            raise SyntaxError("Error in URI '%s' or database not supported" % self._dbname)
                        # notice that driver args or {} else driver_args
                        # defaults to {} global, not correct
                        kwargs = dict(db=self,uri=uri,
                                      pool_size=pool_size,
                                      folder=folder,
                                      db_codec=db_codec,
                                      credential_decoder=credential_decoder,
                                      driver_args=driver_args or {},
                                      adapter_args=adapter_args or {},
                                      do_connect=do_connect,
                                      after_connection=after_connection,
                                      entity_quoting=entity_quoting)
                        self._adapter = ADAPTERS[self._dbname](**kwargs)
                        types = ADAPTERS[self._dbname].types
                        # copy so multiple DAL() possible
                        self._adapter.types = copy.copy(types)
                        self._adapter.build_parsemap()
                        self._adapter.ignore_field_case = ignore_field_case
                        if bigint_id:
                            if 'big-id' in types and 'reference' in types:
                                self._adapter.types['id'] = types['big-id']
                                self._adapter.types['reference'] = types['big-reference']
                        connected = True
                        break
                    except SyntaxError:
                        raise
                    except Exception:
                        tb = traceback.format_exc()
                        self.logger.debug('DEBUG: connect attempt %i, connection error:\n%s' % (k, tb))
                if connected:
                    break
                else:
                    time.sleep(1)
            if not connected:
                raise RuntimeError("Failure to connect, tried %d times:\n%s" % (attempts, tb))
        else:
            self._adapter = BaseAdapter(db=self,pool_size=0,
                                        uri='None',folder=folder,
                                        db_codec=db_codec, after_connection=after_connection,
                                        entity_quoting=entity_quoting)
            migrate = fake_migrate = False
        adapter = self._adapter
        self._uri_hash = table_hash or hashlib_md5(adapter.uri).hexdigest()
        self.check_reserved = check_reserved
        if self.check_reserved:
            from .contrib.reserved_sql_keywords import ADAPTERS as RSK
            self.RSK = RSK
        self._migrate = migrate
        self._fake_migrate = fake_migrate
        self._migrate_enabled = migrate_enabled
        self._fake_migrate_all = fake_migrate_all
        GLOBALS['serializers'] = self.serializers
        if auto_import or tables:
            self.import_table_definitions(adapter.folder,
                                          tables=tables)

    @property
    def tables(self):
        return self._tables

    def import_table_definitions(self, path, migrate=False,
                                 fake_migrate=False, tables=None):
        if tables:
            for table in tables:
                self.define_table(**table)
        else:
            pattern = pjoin(path,self._uri_hash+'_*.table')
            for filename in glob.glob(pattern):
                tfile = self._adapter.file_open(filename, 'r')
                try:
                    sql_fields = pickle.load(tfile)
                    name = filename[len(pattern)-7:-6]
                    mf = [(value['sortable'],
                           Field(key,
                                 type=value['type'],
                                 length=value.get('length',None),
                                 notnull=value.get('notnull',False),
                                 unique=value.get('unique',False))) \
                              for key, value in sql_fields.iteritems()]
                    mf.sort(lambda a,b: cmp(a[0],b[0]))
                    self.define_table(name,*[item[1] for item in mf],
                                      **dict(migrate=migrate,
                                             fake_migrate=fake_migrate))
                finally:
                    self._adapter.file_close(tfile)

    def check_reserved_keyword(self, name):
        """
        Validates `name` against SQL keywords
        Uses self.check_reserve which is a list of operators to use.
        """
        for backend in self.check_reserved:
            if name.upper() in self.RSK[backend]:
                raise SyntaxError(
                    'invalid table/column name "%s" is a "%s" reserved SQL/NOSQL keyword' % (name, backend.upper()))

    def parse_as_rest(self,patterns,args,vars,queries=None,nested_select=True):
        """
        Example:
            Use as::

                db.define_table('person',Field('name'),Field('info'))
                db.define_table('pet',
                    Field('ownedby',db.person),
                    Field('name'),Field('info')
                )

                @request.restful()
                def index():
                    def GET(*args,**vars):
                        patterns = [
                            "/friends[person]",
                            "/{person.name}/:field",
                            "/{person.name}/pets[pet.ownedby]",
                            "/{person.name}/pets[pet.ownedby]/{pet.name}",
                            "/{person.name}/pets[pet.ownedby]/{pet.name}/:field",
                            ("/dogs[pet]", db.pet.info=='dog'),
                            ("/dogs[pet]/{pet.name.startswith}", db.pet.info=='dog'),
                            ]
                        parser = db.parse_as_rest(patterns,args,vars)
                        if parser.status == 200:
                            return dict(content=parser.response)
                        else:
                            raise HTTP(parser.status,parser.error)

                    def POST(table_name,**vars):
                        if table_name == 'person':
                            return db.person.validate_and_insert(**vars)
                        elif table_name == 'pet':
                            return db.pet.validate_and_insert(**vars)
                        else:
                            raise HTTP(400)
                    return locals()
        """

        db = self
        re1 = REGEX_SEARCH_PATTERN
        re2 = REGEX_SQUARE_BRACKETS

        def auto_table(table,base='',depth=0):
            patterns = []
            for field in db[table].fields:
                if base:
                    tag = '%s/%s' % (base,field.replace('_','-'))
                else:
                    tag = '/%s/%s' % (table.replace('_','-'),field.replace('_','-'))
                f = db[table][field]
                if not f.readable: continue
                if f.type=='id' or 'slug' in field or f.type.startswith('reference'):
                    tag += '/{%s.%s}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                elif f.type.startswith('boolean'):
                    tag += '/{%s.%s}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                elif f.type in ('float','double','integer','bigint'):
                    tag += '/{%s.%s.ge}/{%s.%s.lt}' % (table,field,table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                elif f.type.startswith('list:'):
                    tag += '/{%s.%s.contains}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                elif f.type in ('date','datetime'):
                    tag+= '/{%s.%s.year}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                    tag+='/{%s.%s.month}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                    tag+='/{%s.%s.day}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                if f.type in ('datetime','time'):
                    tag+= '/{%s.%s.hour}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                    tag+='/{%s.%s.minute}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                    tag+='/{%s.%s.second}' % (table,field)
                    patterns.append(tag)
                    patterns.append(tag+'/:field')
                if depth>0:
                    for f in db[table]._referenced_by:
                        tag+='/%s[%s.%s]' % (table,f.tablename,f.name)
                        patterns.append(tag)
                        patterns += auto_table(table,base=tag,depth=depth-1)
            return patterns

        if patterns == 'auto':
            patterns=[]
            for table in db.tables:
                if not table.startswith('auth_'):
                    patterns.append('/%s[%s]' % (table,table))
                    patterns += auto_table(table,base='',depth=1)
        else:
            i = 0
            while i<len(patterns):
                pattern = patterns[i]
                if not isinstance(pattern,str):
                    pattern = pattern[0]
                tokens = pattern.split('/')
                if tokens[-1].startswith(':auto') and re2.match(tokens[-1]):
                    new_patterns = auto_table(tokens[-1][tokens[-1].find('[')+1:-1],
                                              '/'.join(tokens[:-1]))
                    patterns = patterns[:i]+new_patterns+patterns[i+1:]
                    i += len(new_patterns)
                else:
                    i += 1
        if '/'.join(args) == 'patterns':
            return Row({'status':200,'pattern':'list',
                        'error':None,'response':patterns})
        for pattern in patterns:
            basequery, exposedfields = None, []
            if isinstance(pattern,tuple):
                if len(pattern)==2:
                    pattern, basequery = pattern
                elif len(pattern)>2:
                    pattern, basequery, exposedfields = pattern[0:3]
            otable=table=None
            if not isinstance(queries,dict):
                dbset=db(queries)
                if basequery is not None:
                    dbset = dbset(basequery)
            i=0
            tags = pattern[1:].split('/')
            if len(tags)!=len(args):
                continue
            for tag in tags:
                if re1.match(tag):
                    # print 're1:'+tag
                    tokens = tag[1:-1].split('.')
                    table, field = tokens[0], tokens[1]
                    if not otable or table == otable:
                        if len(tokens)==2 or tokens[2]=='eq':
                            query = db[table][field]==args[i]
                        elif tokens[2]=='ne':
                            query = db[table][field]!=args[i]
                        elif tokens[2]=='lt':
                            query = db[table][field]<args[i]
                        elif tokens[2]=='gt':
                            query = db[table][field]>args[i]
                        elif tokens[2]=='ge':
                            query = db[table][field]>=args[i]
                        elif tokens[2]=='le':
                            query = db[table][field]<=args[i]
                        elif tokens[2]=='year':
                            query = db[table][field].year()==args[i]
                        elif tokens[2]=='month':
                            query = db[table][field].month()==args[i]
                        elif tokens[2]=='day':
                            query = db[table][field].day()==args[i]
                        elif tokens[2]=='hour':
                            query = db[table][field].hour()==args[i]
                        elif tokens[2]=='minute':
                            query = db[table][field].minutes()==args[i]
                        elif tokens[2]=='second':
                            query = db[table][field].seconds()==args[i]
                        elif tokens[2]=='startswith':
                            query = db[table][field].startswith(args[i])
                        elif tokens[2]=='contains':
                            query = db[table][field].contains(args[i])
                        else:
                            raise RuntimeError("invalid pattern: %s" % pattern)
                        if len(tokens)==4 and tokens[3]=='not':
                            query = ~query
                        elif len(tokens)>=4:
                            raise RuntimeError("invalid pattern: %s" % pattern)
                        if not otable and isinstance(queries,dict):
                            dbset = db(queries[table])
                            if basequery is not None:
                                dbset = dbset(basequery)
                        dbset=dbset(query)
                    else:
                        raise RuntimeError("missing relation in pattern: %s" % pattern)
                elif re2.match(tag) and args[i]==tag[:tag.find('[')]:
                    ref = tag[tag.find('[')+1:-1]
                    if '.' in ref and otable:
                        table,field = ref.split('.')
                        selfld = '_id'
                        if db[table][field].type.startswith('reference '):
                            refs = [ x.name for x in db[otable] if x.type == db[table][field].type ]
                        else:
                            refs = [ x.name for x in db[table]._referenced_by if x.tablename==otable ]
                        if refs:
                            selfld = refs[0]
                        if nested_select:
                            try:
                                dbset=db(db[table][field].belongs(dbset._select(db[otable][selfld])))
                            except ValueError:
                                return Row({'status':400,'pattern':pattern,
                                            'error':'invalid path','response':None})
                        else:
                            items = [item.id for item in dbset.select(db[otable][selfld])]
                            dbset=db(db[table][field].belongs(items))
                    else:
                        table = ref
                        if not otable and isinstance(queries,dict):
                            dbset = db(queries[table])
                        dbset=dbset(db[table])
                elif tag==':field' and table:
                    # print 're3:'+tag
                    field = args[i]
                    if not field in db[table]: break
                    # hand-built patterns should respect .readable=False as well
                    if not db[table][field].readable:
                        return Row({'status':418,'pattern':pattern,
                                    'error':'I\'m a teapot','response':None})
                    try:
                        distinct = vars.get('distinct', False) == 'True'
                        offset = long(vars.get('offset',None) or 0)
                        limits = (offset,long(vars.get('limit',None) or 1000)+offset)
                    except ValueError:
                        return Row({'status':400,'error':'invalid limits','response':None})
                    items =  dbset.select(db[table][field], distinct=distinct, limitby=limits)
                    if items:
                        return Row({'status':200,'response':items,
                                    'pattern':pattern})
                    else:
                        return Row({'status':404,'pattern':pattern,
                                    'error':'no record found','response':None})
                elif tag != args[i]:
                    break
                otable = table
                i += 1
                if i == len(tags) and table:
                    if hasattr(db[table], '_id'):
                        ofields = vars.get('order', db[table]._id.name).split('|')
                    else:
                        ofields = vars.get('order', db[table]._primarykey[0]).split('|')
                    try:
                        orderby = [db[table][f] if not f.startswith('~') else ~db[table][f[1:]] for f in ofields]
                    except (KeyError, AttributeError):
                        return Row({'status':400,'error':'invalid orderby','response':None})
                    if exposedfields:
                        fields = [field for field in db[table] if str(field).split('.')[-1] in exposedfields and field.readable]
                    else:
                        fields = [field for field in db[table] if field.readable]
                    count = dbset.count()
                    try:
                        offset = long(vars.get('offset',None) or 0)
                        limits = (offset,long(vars.get('limit',None) or 1000)+offset)
                    except ValueError:
                        return Row({'status':400,'error':'invalid limits','response':None})
                    #if count > limits[1]-limits[0]:
                    #    return Row({'status':400,'error':'too many records','response':None})
                    try:
                        response = dbset.select(limitby=limits,orderby=orderby,*fields)
                    except ValueError:
                        return Row({'status':400,'pattern':pattern,
                                    'error':'invalid path','response':None})
                    return Row({'status':200,'response':response,
                                'pattern':pattern,'count':count})
        return Row({'status':400,'error':'no matching pattern','response':None})

    def define_table(
        self,
        tablename,
        *fields,
        **args
        ):
        if not fields and 'fields' in args:
            fields = args.get('fields',())
        if not isinstance(tablename, str):
            if isinstance(tablename, unicode):
                try:
                    tablename = str(tablename)
                except UnicodeEncodeError:
                    raise SyntaxError("invalid unicode table name")
            else:
                raise SyntaxError("missing table name")
        elif hasattr(self,tablename) or tablename in self.tables:
            if args.get('redefine',False):
                delattr(self, tablename)
            else:
                raise SyntaxError('table already defined: %s' % tablename)
        elif tablename.startswith('_') or hasattr(self,tablename) or \
                REGEX_PYTHON_KEYWORDS.match(tablename):
            raise SyntaxError('invalid table name: %s' % tablename)
        elif self.check_reserved:
            self.check_reserved_keyword(tablename)
        else:
            invalid_args = set(args)-TABLE_ARGS
            if invalid_args:
                raise SyntaxError('invalid table "%s" attributes: %s' \
                    % (tablename,invalid_args))
        if self._lazy_tables and tablename not in self._LAZY_TABLES:
            self._LAZY_TABLES[tablename] = (tablename,fields,args)
            table = None
        else:
            table = self.lazy_define_table(tablename,*fields,**args)
        if not tablename in self.tables:
            self.tables.append(tablename)
        return table

    def lazy_define_table(
        self,
        tablename,
        *fields,
        **args
        ):
        args_get = args.get
        common_fields = self._common_fields
        if common_fields:
            fields = list(fields) + list(common_fields)

        table_class = args_get('table_class',Table)
        table = table_class(self, tablename, *fields, **args)
        table._actual = True
        self[tablename] = table
        # must follow above line to handle self references
        table._create_references()
        for field in table:
            if field.requires == DEFAULT:
                field.requires = auto_validators(field)
            if field.represent == DEFAULT:
                field.represent = auto_represent(field)

        migrate = self._migrate_enabled and args_get('migrate',self._migrate)
        if migrate and not self._uri in (None,'None') \
                or self._adapter.dbengine=='google:datastore':
            fake_migrate = self._fake_migrate_all or \
                args_get('fake_migrate',self._fake_migrate)
            polymodel = args_get('polymodel',None)
            try:
                GLOBAL_LOCKER.acquire()
                self._lastsql = self._adapter.create_table(
                    table,migrate=migrate,
                    fake_migrate=fake_migrate,
                    polymodel=polymodel)
            finally:
                GLOBAL_LOCKER.release()
        else:
            table._dbt = None
        on_define = args_get('on_define',None)
        if on_define: on_define(table)
        return table

    def as_dict(self, flat=False, sanitize=True):
        db_uid = uri = None
        if not sanitize:
            uri, db_uid = (self._uri, self._db_uid)
        db_as_dict = dict(tables=[], uri=uri, db_uid=db_uid,
                          **dict([(k, getattr(self, "_" + k, None))
                          for k in 'pool_size','folder','db_codec',
                          'check_reserved','migrate','fake_migrate',
                          'migrate_enabled','fake_migrate_all',
                          'decode_credentials','driver_args',
                          'adapter_args', 'attempts',
                          'bigint_id','debug','lazy_tables',
                          'do_connect']))
        for table in self:
            db_as_dict["tables"].append(table.as_dict(flat=flat,
                                        sanitize=sanitize))
        return db_as_dict

    def has_serializer(self, name):
        return hasattr(self.serializers, name) and \
            callable(getattr(self.serializers, 'loads_json'))

    def serialize(self, name, *args, **kwargs):
        return getattr(self.serializers, name)(*args, **kwargs)

    def as_xml(self, sanitize=True):
        if not self.has_serializer('xml'):
            raise ImportError("No xml serializers available")
        d = self.as_dict(flat=True, sanitize=sanitize)
        return self.serialize('xml', d)

    def as_json(self, sanitize=True):
        if not self.has_serializer('json'):
            raise ImportError("No json serializers available")
        d = self.as_dict(flat=True, sanitize=sanitize)
        return self.serialize('json', d)

    def as_yaml(self, sanitize=True):
        if not self.has_serializer('yaml'):
            raise ImportError("No YAML serializers available")
        d = self.as_dict(flat=True, sanitize=sanitize)
        return self.serialize('yaml', d)

    def __contains__(self, tablename):
        try:
            return tablename in self.tables
        except AttributeError:
            # The instance has no .tables attribute yet
            return False

    has_key = __contains__

    def get(self, key, default=None):
        return self.__dict__.get(key, default)

    def __iter__(self):
        for tablename in self.tables:
            yield self[tablename]

    def __getitem__(self, key):
        return self.__getattr__(str(key))

    def __getattr__(self, key):
        if ogetattr(self,'_lazy_tables') and \
                key in ogetattr(self,'_LAZY_TABLES'):
            tablename, fields, args = self._LAZY_TABLES.pop(key)
            return self.lazy_define_table(tablename,*fields,**args)
        return ogetattr(self, key)

    def __setitem__(self, key, value):
        osetattr(self, str(key), value)

    def __setattr__(self, key, value):
        if key[:1]!='_' and key in self:
            raise SyntaxError(
                'Object %s exists and cannot be redefined' % key)
        osetattr(self,key,value)

    __delitem__ = object.__delattr__

    def __repr__(self):
        if hasattr(self,'_uri'):
            return '<DAL uri="%s">' % hide_password(self._adapter.uri)
        else:
            return '<DAL db_uid="%s">' % self._db_uid

    def smart_query(self,fields,text):
        return Set(self, smart_query(fields,text))

    def __call__(self, query=None, ignore_common_filters=None):
        if isinstance(query,Table):
            query = self._adapter.id_query(query)
        elif isinstance(query,Field):
            query = query!=None
        elif isinstance(query, dict):
            icf = query.get("ignore_common_filters")
            if icf: ignore_common_filters = icf
        return Set(self, query, ignore_common_filters=ignore_common_filters)

    def commit(self):
        self._adapter.commit()

    def rollback(self):
        self._adapter.rollback()

    def close(self):
        self._adapter.close()
        if self._db_uid in THREAD_LOCAL.db_instances:
            db_group = THREAD_LOCAL.db_instances[self._db_uid]
            db_group.remove(self)
            if not db_group:
                del THREAD_LOCAL.db_instances[self._db_uid]

    def executesql(self, query, placeholders=None, as_dict=False,
                   fields=None, colnames=None, as_ordered_dict=False):
        """
        Executes an arbitrary query

        Args:
            query (str): the query to submit to the backend
            placeholders: is optional and will always be None.
                If using raw SQL with placeholders, placeholders may be
                a sequence of values to be substituted in
                or, (if supported by the DB driver), a dictionary with keys
                matching named placeholders in your SQL.
            as_dict: will always be None when using DAL.
                If using raw SQL can be set to True and the results cursor
                returned by the DB driver will be converted to a sequence of
                dictionaries keyed with the db field names. Results returned
                with as_dict=True are the same as those returned when applying
                .to_list() to a DAL query.  If "as_ordered_dict"=True the
                behaviour is the same as when "as_dict"=True with the keys
                (field names) guaranteed to be in the same order as returned
                by the select name executed on the database.
            fields: list of DAL Fields that match the fields returned from the
                DB. The Field objects should be part of one or more Table
                objects defined on the DAL object. The "fields" list can include
                one or more DAL Table objects in addition to or instead of
                including Field objects, or it can be just a single table
                (not in a list). In that case, the Field objects will be
                extracted from the table(s).

                Note:
                    if either `fields` or `colnames` is provided, the results
                    will be converted to a DAL `Rows` object using the
                    `db._adapter.parse()` method
            colnames: list of field names in tablename.fieldname format

        Note:
            It is also possible to specify both "fields" and the associated
            "colnames". In that case, "fields" can also include DAL Expression
            objects in addition to Field objects. For Field objects in "fields",
            the associated "colnames" must still be in tablename.fieldname
            format. For Expression objects in "fields", the associated
            "colnames" can be any arbitrary labels.

        DAL Table objects referred to by "fields" or "colnames" can be dummy
        tables and do not have to represent any real tables in the database.
        Also, note that the "fields" and "colnames" must be in the
        same order as the fields in the results cursor returned from the DB.

        """
        adapter = self._adapter
        if placeholders:
            adapter.execute(query, placeholders)
        else:
            adapter.execute(query)
        if as_dict or as_ordered_dict:
            if not hasattr(adapter.cursor,'description'):
                raise RuntimeError("database does not support executesql(...,as_dict=True)")
            # Non-DAL legacy db query, converts cursor results to dict.
            # sequence of 7-item sequences. each sequence tells about a column.
            # first item is always the field name according to Python Database API specs
            columns = adapter.cursor.description
            # reduce the column info down to just the field names
            fields = colnames or [f[0] for f in columns]
            if len(fields) != len(set(fields)):
                raise RuntimeError("Result set includes duplicate column names. Specify unique column names using the 'colnames' argument")

            # will hold our finished resultset in a list
            data = adapter._fetchall()
            # convert the list for each row into a dictionary so it's
            # easier to work with. row['field_name'] rather than row[0]
            if as_ordered_dict:
                _dict = OrderedDict
            else:
                _dict = dict
            return [_dict(zip(fields,row)) for row in data]
        try:
            data = adapter._fetchall()
        except:
            return None
        if fields or colnames:
            fields = [] if fields is None else fields
            if not isinstance(fields, list):
                fields = [fields]
            extracted_fields = []
            for field in fields:
                if isinstance(field, Table):
                    extracted_fields.extend([f for f in field])
                else:
                    extracted_fields.append(field)
            if not colnames:
                colnames = ['%s.%s' % (f.tablename, f.name)
                            for f in extracted_fields]
            data = adapter.parse(
                data, fields=extracted_fields, colnames=colnames)
        return data

    def _remove_references_to(self, thistable):
        for table in self:
            table._referenced_by = [field for field in table._referenced_by
                                    if not field.table==thistable]

    def has_representer(self, name):
        return callable(self.representers.get(name))

    def represent(self, name, *args, **kwargs):
        return self.representers[name](*args, **kwargs)

    def export_to_csv_file(self, ofile, *args, **kwargs):
        step = long(kwargs.get('max_fetch_rows,',500))
        write_colnames = kwargs['write_colnames'] = \
            kwargs.get("write_colnames", True)
        for table in self.tables:
            ofile.write('TABLE %s\r\n' % table)
            query = self._adapter.id_query(self[table])
            nrows = self(query).count()
            kwargs['write_colnames'] = write_colnames
            for k in range(0,nrows,step):
                self(query).select(limitby=(k,k+step)).export_to_csv_file(
                    ofile, *args, **kwargs)
                kwargs['write_colnames'] = False
            ofile.write('\r\n\r\n')
        ofile.write('END')

    def import_from_csv_file(self, ifile, id_map=None, null='<NULL>',
                             unique='uuid', map_tablenames=None,
                             ignore_missing_tables=False,
                             *args, **kwargs):
        #if id_map is None: id_map={}
        id_offset = {} # only used if id_map is None
        map_tablenames = map_tablenames or {}
        for line in ifile:
            line = line.strip()
            if not line:
                continue
            elif line == 'END':
                return
            elif not line.startswith('TABLE ') or \
                    not line[6:] in self.tables:
                raise SyntaxError('invalid file format')
            else:
                tablename = line[6:]
                tablename = map_tablenames.get(tablename,tablename)
                if tablename is not None and tablename in self.tables:
                    self[tablename].import_from_csv_file(
                        ifile, id_map, null, unique, id_offset,
                        *args, **kwargs)
                elif tablename is None or ignore_missing_tables:
                    # skip all non-empty lines
                    for line in ifile:
                        if not line.strip():
                            break
                else:
                    raise RuntimeError("Unable to import table that does not exist.\nTry db.import_from_csv_file(..., map_tablenames={'table':'othertable'},ignore_missing_tables=True)")


def DAL_unpickler(db_uid):
    return DAL('<zombie>', db_uid=db_uid)


def DAL_pickler(db):
    return DAL_unpickler, (db._db_uid,)

copyreg.pickle(DAL, DAL_pickler, DAL_unpickler)
