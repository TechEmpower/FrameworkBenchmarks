#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of web2py Web Framework (Copyrighted, 2007-2009).
Developed by Massimo Di Pierro <mdipierro@cs.depaul.edu> and
Robin B <robi123@gmail.com>.
License: LGPLv3
"""

__all__ = ['MEMDB', 'Field']

import re
import sys
import os
import types
import datetime
import thread
import cStringIO
import csv
import copy
import gluon.validators as validators
from gluon.utils import web2py_uuid
from gluon.storage import Storage
from gluon import SQLTABLE
import random

SQL_DIALECTS = {'memcache': {
    'boolean': bool,
    'string': unicode,
    'text': unicode,
    'password': unicode,
    'blob': unicode,
    'upload': unicode,
    'integer': long,
    'double': float,
    'date': datetime.date,
    'time': datetime.time,
    'datetime': datetime.datetime,
    'id': int,
    'reference': int,
    'lower': None,
    'upper': None,
    'is null': 'IS NULL',
    'is not null': 'IS NOT NULL',
    'extract': None,
    'left join': None,
}}


def cleanup(text):
    if re.compile('[^0-9a-zA-Z_]').findall(text):
        raise SyntaxError('Can\'t cleanup \'%s\': only [0-9a-zA-Z_] allowed in table and field names' % text)
    return text


def assert_filter_fields(*fields):
    for field in fields:
        if isinstance(field, (Field, Expression)) and field.type\
                in ['text', 'blob']:
            raise SyntaxError('AppEngine does not index by: %s'
                              % field.type)


def dateobj_to_datetime(object):

    # convert dates,times to datetimes for AppEngine

    if isinstance(object, datetime.date):
        object = datetime.datetime(object.year, object.month,
                                   object.day)
    if isinstance(object, datetime.time):
        object = datetime.datetime(
            1970,
            1,
            1,
            object.hour,
            object.minute,
            object.second,
            object.microsecond,
        )
    return object


def sqlhtml_validators(field_type, length):
    v = {
        'boolean': [],
        'string': validators.IS_LENGTH(length),
        'text': [],
        'password': validators.IS_LENGTH(length),
        'blob': [],
        'upload': [],
        'double': validators.IS_FLOAT_IN_RANGE(-1e100, 1e100),
        'integer': validators.IS_INT_IN_RANGE(-1e100, 1e100),
        'date': validators.IS_DATE(),
        'time': validators.IS_TIME(),
        'datetime': validators.IS_DATETIME(),
        'reference': validators.IS_INT_IN_RANGE(0, 1e100),
    }
    try:
        return v[field_type[:9]]
    except KeyError:
        return []


class DALStorage(dict):

    """
    a dictionary that let you do d['a'] as well as d.a
    """

    def __getattr__(self, key):
        return self[key]

    def __setattr__(self, key, value):
        if key in self:
            raise SyntaxError(
                'Object \'%s\'exists and cannot be redefined' % key)
        self[key] = value

    def __repr__(self):
        return '<DALStorage ' + dict.__repr__(self) + '>'


class SQLCallableList(list):

    def __call__(self):
        return copy.copy(self)


class MEMDB(DALStorage):

    """
    an instance of this class represents a database connection

    Example::

       db=MEMDB(Client())
       db.define_table('tablename',Field('fieldname1'),
                                   Field('fieldname2'))
    """

    def __init__(self, client):
        self._dbname = 'memdb'
        self['_lastsql'] = ''
        self.tables = SQLCallableList()
        self._translator = SQL_DIALECTS['memcache']
        self.client = client

    def define_table(
        self,
        tablename,
        *fields,
        **args
    ):
        tablename = cleanup(tablename)
        if tablename in dir(self) or tablename[0] == '_':
            raise SyntaxError('invalid table name: %s' % tablename)
        if not tablename in self.tables:
            self.tables.append(tablename)
        else:
            raise SyntaxError('table already defined: %s' % tablename)
        t = self[tablename] = Table(self, tablename, *fields)
        t._create()
        return t

    def __call__(self, where=''):
        return Set(self, where)


class SQLALL(object):

    def __init__(self, table):
        self.table = table


class Table(DALStorage):

    """
    an instance of this class represents a database table

    Example::

        db=MEMDB(Client())
        db.define_table('users',Field('name'))
        db.users.insert(name='me')
    """

    def __init__(
        self,
        db,
        tablename,
        *fields
    ):
        self._db = db
        self._tablename = tablename
        self.fields = SQLCallableList()
        self._referenced_by = []
        fields = list(fields)
        fields.insert(0, Field('id', 'id'))
        for field in fields:
            self.fields.append(field.name)
            self[field.name] = field
            field._tablename = self._tablename
            field._table = self
            field._db = self._db
        self.ALL = SQLALL(self)

    def _create(self):
        fields = []
        myfields = {}
        for k in self.fields:
            field = self[k]
            attr = {}
            if not field.type[:9] in ['id', 'reference']:
                if field.notnull:
                    attr = dict(required=True)
            if field.type[:2] == 'id':
                continue
            if field.type[:9] == 'reference':
                referenced = field.type[10:].strip()
                if not referenced:
                    raise SyntaxError('Table %s: reference \'%s\' to nothing!' % (
                        self._tablename, k))
                if not referenced in self._db:
                    raise SyntaxError(
                        'Table: table %s does not exist' % referenced)
                referee = self._db[referenced]
                ftype = \
                    self._db._translator[field.type[:9]](
                        self._db[referenced]._tableobj)
                if self._tablename in referee.fields:  # ## THIS IS OK
                    raise SyntaxError('Field: table \'%s\' has same name as a field '
                                      'in referenced table \'%s\'' % (
                                          self._tablename, referenced))
                self._db[referenced]._referenced_by.append((self._tablename,
                                                            field.name))
            elif not field.type in self._db._translator\
                    or not self._db._translator[field.type]:
                raise SyntaxError('Field: unknown field type %s' % field.type)
        self._tableobj = self._db.client
        return None

    def create(self):

        # nothing to do, here for backward compatility

        pass

    def drop(self):

        # nothing to do, here for backward compatibility

        self._db(self.id > 0).delete()


    def insert(self, **fields):
        # Checks 3 times that the id is new. 3 times is enough!
        for i in range(3):
            id = self._create_id()
            if self.get(id) is None and self.update(id, **fields):
                return long(id)
        else:
            raise RuntimeError("Too many ID conflicts")

    def get(self, id):
        val = self._tableobj.get(self._id_to_key(id))
        if val:
            return Storage(val)
        else:
            return None

    def update(self, id, **fields):
        for field in fields:
            if not field in fields and self[field].default\
                    is not None:
                fields[field] = self[field].default
            if field in fields:
                fields[field] = obj_represent(fields[field],
                                              self[field].type, self._db)
        return self._tableobj.set(self._id_to_key(id), fields)

    def delete(self, id):
        return self._tableobj.delete(self._id_to_key(id))

    def _id_to_key(self, id):
        return '__memdb__/t/%s/k/%s' % (self._tablename, str(id))

    def _create_id(self):
        return long(web2py_uuid().replace('-',''),16)

    def __str__(self):
        return self._tablename

    def __call__(self, id, **kwargs):
        record = self.get(id)
        if record is None:
          return None
        if kwargs and any(record[key]!=kwargs[key] for key in kwargs):
            return None
        return record

class Expression(object):

    def __init__(
        self,
        name,
        type='string',
        db=None,
    ):
        (self.name, self.type, self._db) = (name, type, db)

    def __str__(self):
        return self.name

    def __or__(self, other):  # for use in sortby
        assert_filter_fields(self, other)
        return Expression(self.name + '|' + other.name, None, None)

    def __invert__(self):
        assert_filter_fields(self)
        return Expression('-' + self.name, self.type, None)

    # for use in Query

    def __eq__(self, value):
        return Query(self, '=', value)

    def __ne__(self, value):
        return Query(self, '!=', value)

    def __lt__(self, value):
        return Query(self, '<', value)

    def __le__(self, value):
        return Query(self, '<=', value)

    def __gt__(self, value):
        return Query(self, '>', value)

    def __ge__(self, value):
        return Query(self, '>=', value)

    # def like(self,value): return Query(self,' LIKE ',value)
    # def belongs(self,value): return Query(self,' IN ',value)
    # for use in both Query and sortby

    def __add__(self, other):
        return Expression('%s+%s' % (self, other), 'float', None)

    def __sub__(self, other):
        return Expression('%s-%s' % (self, other), 'float', None)

    def __mul__(self, other):
        return Expression('%s*%s' % (self, other), 'float', None)

    def __div__(self, other):
        return Expression('%s/%s' % (self, other), 'float', None)


class Field(Expression):

    """
    an instance of this class represents a database field

    example::

        a = Field(name, 'string', length=32, required=False,
                     default=None, requires=IS_NOT_EMPTY(), notnull=False,
                     unique=False, uploadfield=True)

    to be used as argument of GQLDB.define_table

    allowed field types:
    string, boolean, integer, double, text, blob,
    date, time, datetime, upload, password

    strings must have a length or 512 by default.
    fields should have a default or they will be required in SQLFORMs
    the requires argument are used to validate the field input in SQLFORMs

    """

    def __init__(
        self,
        fieldname,
        type='string',
        length=None,
        default=None,
        required=False,
        requires=sqlhtml_validators,
        ondelete='CASCADE',
        notnull=False,
        unique=False,
        uploadfield=True,
    ):

        self.name = cleanup(fieldname)
        if fieldname in dir(Table) or fieldname[0] == '_':
            raise SyntaxError('Field: invalid field name: %s' % fieldname)
        if isinstance(type, Table):
            type = 'reference ' + type._tablename
        if not length:
            length = 512
        self.type = type  # 'string', 'integer'
        self.length = length  # the length of the string
        self.default = default  # default value for field
        self.required = required  # is this field required
        self.ondelete = ondelete.upper()  # this is for reference fields only
        self.notnull = notnull
        self.unique = unique
        self.uploadfield = uploadfield
        if requires == sqlhtml_validators:
            requires = sqlhtml_validators(type, length)
        elif requires is None:
            requires = []
        self.requires = requires  # list of validators

    def formatter(self, value):
        if value is None or not self.requires:
            return value
        if not isinstance(self.requires, (list, tuple)):
            requires = [self.requires]
        else:
            requires = copy.copy(self.requires)
        requires.reverse()
        for item in requires:
            if hasattr(item, 'formatter'):
                value = item.formatter(value)
        return value

    def __str__(self):
        return '%s.%s' % (self._tablename, self.name)


MEMDB.Field = Field  # ## required by gluon/globals.py session.connect


def obj_represent(object, fieldtype, db):
    if object is not None:
        if fieldtype == 'date' and not isinstance(object,
                                                  datetime.date):
            (y, m, d) = [int(x) for x in str(object).strip().split('-')]
            object = datetime.date(y, m, d)
        elif fieldtype == 'time' and not isinstance(object, datetime.time):
            time_items = [int(x) for x in str(object).strip().split(':')[:3]]
            if len(time_items) == 3:
                (h, mi, s) = time_items
            else:
                (h, mi, s) = time_items + [0]
            object = datetime.time(h, mi, s)
        elif fieldtype == 'datetime' and not isinstance(object,
                                                        datetime.datetime):
            (y, m, d) = [int(x) for x in
                         str(object)[:10].strip().split('-')]
            time_items = [int(x) for x in
                          str(object)[11:].strip().split(':')[:3]]
            if len(time_items) == 3:
                (h, mi, s) = time_items
            else:
                (h, mi, s) = time_items + [0]
            object = datetime.datetime(
                y,
                m,
                d,
                h,
                mi,
                s,
            )
        elif fieldtype == 'integer' and not isinstance(object, long):
            object = long(object)

    return object


class QueryException:

    def __init__(self, **a):
        self.__dict__ = a


class Query(object):

    """
    A query object necessary to define a set.
    It can be stored or can be passed to GQLDB.__call__() to obtain a Set

    Example:
    query=db.users.name=='Max'
    set=db(query)
    records=set.select()
    """

    def __init__(
        self,
        left,
        op=None,
        right=None,
    ):
        if isinstance(right, (Field, Expression)):
            raise SyntaxError(
                'Query: right side of filter must be a value or entity')
        if isinstance(left, Field) and left.name == 'id':
            if op == '=':
                self.get_one = QueryException(
                    tablename=left._tablename, id=long(right or 0))
                return
            else:
                raise SyntaxError('only equality by id is supported')
        raise SyntaxError('not supported')

    def __str__(self):
        return str(self.left)


class Set(object):

    """
    As Set represents a set of records in the database,
    the records are identified by the where=Query(...) object.
    normally the Set is generated by GQLDB.__call__(Query(...))

    given a set, for example
       set=db(db.users.name=='Max')
    you can:
       set.update(db.users.name='Massimo')
       set.delete() # all elements in the set
       set.select(orderby=db.users.id,groupby=db.users.name,limitby=(0,10))
    and take subsets:
       subset=set(db.users.id<5)
    """

    def __init__(self, db, where=None):
        self._db = db
        self._tables = []
        self.filters = []
        if hasattr(where, 'get_all'):
            self.where = where
            self._tables.insert(0, where.get_all)
        elif hasattr(where, 'get_one') and isinstance(where.get_one,
                                                      QueryException):
            self.where = where.get_one
        else:

            # find out which tables are involved

            if isinstance(where, Query):
                self.filters = where.left
            self.where = where
            self._tables = [field._tablename for (field, op, val) in
                            self.filters]

    def __call__(self, where):
        if isinstance(self.where, QueryException) or isinstance(where,
                                                                QueryException):
            raise SyntaxError('neither self.where nor where can be a QueryException instance')
        if self.where:
            return Set(self._db, self.where & where)
        else:
            return Set(self._db, where)

    def _get_table_or_raise(self):
        tablenames = list(set(self._tables))  # unique
        if len(tablenames) < 1:
            raise SyntaxError('Set: no tables selected')
        if len(tablenames) > 1:
            raise SyntaxError('Set: no join in appengine')
        return self._db[tablenames[0]]._tableobj

    def _getitem_exception(self):
        (tablename, id) = (self.where.tablename, self.where.id)
        fields = self._db[tablename].fields
        self.colnames = ['%s.%s' % (tablename, t) for t in fields]
        item = self._db[tablename].get(id)
        return (item, fields, tablename, id)

    def _select_except(self):
        (item, fields, tablename, id) = self._getitem_exception()
        if not item:
            return []
        new_item = []
        for t in fields:
            if t == 'id':
                new_item.append(long(id))
            else:
                new_item.append(getattr(item, t))
        r = [new_item]
        return Rows(self._db, r, *self.colnames)

    def select(self, *fields, **attributes):
        """
        Always returns a Rows object, even if it may be empty
        """

        if isinstance(self.where, QueryException):
            return self._select_except()
        else:
            raise SyntaxError('select arguments not supported')

    def count(self):
        return len(self.select())

    def delete(self):
        if isinstance(self.where, QueryException):
            (item, fields, tablename, id) = self._getitem_exception()
            if not item:
                return
            self._db[tablename].delete(id)
        else:
            raise Exception('deletion not implemented')

    def update(self, **update_fields):
        if isinstance(self.where, QueryException):
            (item, fields, tablename, id) = self._getitem_exception()
            if not item:
                return
            for (key, value) in update_fields.items():
                setattr(item, key, value)
            self._db[tablename].update(id, **item)
        else:
            raise Exception('update not implemented')


def update_record(
    t,
    s,
    id,
    a,
):
    item = s.get(id)
    for (key, value) in a.items():
        t[key] = value
        setattr(item, key, value)
    s.update(id, **item)


class Rows(object):

    """
    A wrapper for the return value of a select. It basically represents a table.
    It has an iterator and each row is represented as a dictionary.
    """

    # ## this class still needs some work to care for ID/OID

    def __init__(
        self,
        db,
        response,
        *colnames
    ):
        self._db = db
        self.colnames = colnames
        self.response = response

    def __len__(self):
        return len(self.response)

    def __getitem__(self, i):
        if i >= len(self.response) or i < 0:
            raise SyntaxError('Rows: no such row: %i' % i)
        if len(self.response[0]) != len(self.colnames):
            raise SyntaxError('Rows: internal error')
        row = DALStorage()
        for j in xrange(len(self.colnames)):
            value = self.response[i][j]
            if isinstance(value, unicode):
                value = value.encode('utf-8')
            packed = self.colnames[j].split('.')
            try:
                (tablename, fieldname) = packed
            except:
                if not '_extra' in row:
                    row['_extra'] = DALStorage()
                row['_extra'][self.colnames[j]] = value
                continue
            table = self._db[tablename]
            field = table[fieldname]
            if not tablename in row:
                row[tablename] = DALStorage()
            if field.type[:9] == 'reference':
                referee = field.type[10:].strip()
                rid = value
                row[tablename][fieldname] = rid
            elif field.type == 'boolean' and value is not None:

                # row[tablename][fieldname]=Set(self._db[referee].id==rid)

                if value == True or value == 'T':
                    row[tablename][fieldname] = True
                else:
                    row[tablename][fieldname] = False
            elif field.type == 'date' and value is not None\
                    and not isinstance(value, datetime.date):
                (y, m, d) = [int(x) for x in
                             str(value).strip().split('-')]
                row[tablename][fieldname] = datetime.date(y, m, d)
            elif field.type == 'time' and value is not None\
                    and not isinstance(value, datetime.time):
                time_items = [int(x) for x in
                              str(value).strip().split(':')[:3]]
                if len(time_items) == 3:
                    (h, mi, s) = time_items
                else:
                    (h, mi, s) = time_items + [0]
                row[tablename][fieldname] = datetime.time(h, mi, s)
            elif field.type == 'datetime' and value is not None\
                    and not isinstance(value, datetime.datetime):
                (y, m, d) = [int(x) for x in
                             str(value)[:10].strip().split('-')]
                time_items = [int(x) for x in
                              str(value)[11:].strip().split(':')[:3]]
                if len(time_items) == 3:
                    (h, mi, s) = time_items
                else:
                    (h, mi, s) = time_items + [0]
                row[tablename][fieldname] = datetime.datetime(
                    y,
                    m,
                    d,
                    h,
                    mi,
                    s,
                )
            else:
                row[tablename][fieldname] = value
            if fieldname == 'id':
                id = row[tablename].id
                row[tablename].update_record = lambda t = row[tablename], \
                    s = self._db[tablename], id = id, **a: update_record(t,
                                                                         s, id, a)
                for (referee_table, referee_name) in \
                        table._referenced_by:
                    s = self._db[referee_table][referee_name]
                    row[tablename][referee_table] = Set(self._db, s
                                                        == id)
        if len(row.keys()) == 1:
            return row[row.keys()[0]]
        return row

    def __iter__(self):
        """
        iterator over records
        """

        for i in xrange(len(self)):
            yield self[i]

    def __str__(self):
        """
        serializes the table into a csv file
        """

        s = cStringIO.StringIO()
        writer = csv.writer(s)
        writer.writerow(self.colnames)
        c = len(self.colnames)
        for i in xrange(len(self)):
            row = [self.response[i][j] for j in xrange(c)]
            for k in xrange(c):
                if isinstance(row[k], unicode):
                    row[k] = row[k].encode('utf-8')
            writer.writerow(row)
        return s.getvalue()

    def xml(self):
        """
        serializes the table using SQLTABLE (if present)
        """

        return SQLTABLE(self).xml()


def test_all():
    """
    How to run from web2py dir:
     export PYTHONPATH=.:YOUR_PLATFORMS_APPENGINE_PATH
     python gluon/contrib/memdb.py

    Setup the UTC timezone and database stubs

    >>> import os
    >>> os.environ['TZ'] = 'UTC'
    >>> import time
    >>> if hasattr(time, 'tzset'):
    ...   time.tzset()
    >>>
    >>> from google.appengine.api import apiproxy_stub_map
    >>> from google.appengine.api.memcache import memcache_stub
    >>> apiproxy_stub_map.apiproxy = apiproxy_stub_map.APIProxyStubMap()
    >>> apiproxy_stub_map.apiproxy.RegisterStub('memcache', memcache_stub.MemcacheServiceStub())

        Create a table with all possible field types
    >>> from google.appengine.api.memcache import Client
    >>> db=MEMDB(Client())
    >>> tmp=db.define_table('users',              Field('stringf','string',length=32,required=True),              Field('booleanf','boolean',default=False),              Field('passwordf','password',notnull=True),              Field('blobf','blob'),              Field('uploadf','upload'),              Field('integerf','integer',unique=True),              Field('doublef','double',unique=True,notnull=True),              Field('datef','date',default=datetime.date.today()),              Field('timef','time'),              Field('datetimef','datetime'),              migrate='test_user.table')

   Insert a field

    >>> user_id = db.users.insert(stringf='a',booleanf=True,passwordf='p',blobf='0A',                       uploadf=None, integerf=5,doublef=3.14,                       datef=datetime.date(2001,1,1),                       timef=datetime.time(12,30,15),                       datetimef=datetime.datetime(2002,2,2,12,30,15))
    >>> user_id != None
    True

    Select all

    # >>> all = db().select(db.users.ALL)

    Drop the table

    # >>> db.users.drop()

    Select many entities

    >>> tmp = db.define_table(\"posts\",              Field('body','text'),              Field('total','integer'),              Field('created_at','datetime'))
    >>> many = 20   #2010 # more than 1000 single fetch limit (it can be slow)
    >>> few = 5
    >>> most = many - few
    >>> 0 < few < most < many
    True
    >>> for i in range(many):
    ...     f=db.posts.insert(body='',                total=i,created_at=datetime.datetime(2008, 7, 6, 14, 15, 42, i))
    >>>

    # test timezones
    >>> class TZOffset(datetime.tzinfo):
    ...   def __init__(self,offset=0):
    ...     self.offset = offset
    ...   def utcoffset(self, dt): return datetime.timedelta(hours=self.offset)
    ...   def dst(self, dt): return datetime.timedelta(0)
    ...   def tzname(self, dt): return 'UTC' + str(self.offset)
    ...
    >>> SERVER_OFFSET = -8
    >>>
    >>> stamp = datetime.datetime(2008, 7, 6, 14, 15, 42, 828201)
    >>> post_id = db.posts.insert(created_at=stamp,body='body1')
    >>> naive_stamp = db(db.posts.id==post_id).select()[0].created_at
    >>> utc_stamp=naive_stamp.replace(tzinfo=TZOffset())
    >>> server_stamp = utc_stamp.astimezone(TZOffset(SERVER_OFFSET))
    >>> stamp == naive_stamp
    True
    >>> utc_stamp == server_stamp
    True
    >>> rows = db(db.posts.id==post_id).select()
    >>> len(rows) == 1
    True
    >>> rows[0].body == 'body1'
    True
    >>> db(db.posts.id==post_id).delete()
    >>> rows = db(db.posts.id==post_id).select()
    >>> len(rows) == 0
    True

    >>> id = db.posts.insert(total='0')   # coerce str to integer
    >>> rows = db(db.posts.id==id).select()
    >>> len(rows) == 1
    True
    >>> rows[0].total == 0
    True

    Examples of insert, select, update, delete

    >>> tmp=db.define_table('person', Field('name'), Field('birth','date'), migrate='test_person.table')
    >>> marco_id=db.person.insert(name=\"Marco\",birth='2005-06-22')
    >>> person_id=db.person.insert(name=\"Massimo\",birth='1971-12-21')
    >>> me=db(db.person.id==person_id).select()[0] # test select
    >>> me.name
    'Massimo'
    >>> db(db.person.id==person_id).update(name='massimo') # test update
    >>> me = db(db.person.id==person_id).select()[0]
    >>> me.name
    'massimo'
    >>> str(me.birth)
    '1971-12-21'

    # resave date to ensure it comes back the same
    >>> me=db(db.person.id==person_id).update(birth=me.birth) # test update
    >>> me = db(db.person.id==person_id).select()[0]
    >>> me.birth
    datetime.date(1971, 12, 21)
    >>> db(db.person.id==marco_id).delete() # test delete
    >>> len(db(db.person.id==marco_id).select())
    0

    Update a single record

    >>> me.update_record(name=\"Max\")
    >>> me.name
    'Max'
    >>> me = db(db.person.id == person_id).select()[0]
    >>> me.name
    'Max'

    """

SQLField = Field
SQLTable = Table
SQLXorable = Expression
SQLQuery = Query
SQLSet = Set
SQLRows = Rows
SQLStorage = DALStorage

if __name__ == '__main__':
    import doctest
    doctest.testmod()
