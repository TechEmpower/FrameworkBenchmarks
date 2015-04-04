# -*- coding: utf-8 -*-
import datetime

from .._globals import IDENTITY
from .._load import json
from ..drivers import couchdb
from ..objects import Field, Query
from ..helpers.classes import SQLALL
from ..helpers.methods import uuid2int
from .base import BaseAdapter, NoSQLAdapter, SELECT_ARGS


class CouchDBAdapter(NoSQLAdapter):
    drivers = ('couchdb',)

    uploads_in_blob = True
    types = {
            'boolean': bool,
            'string': str,
            'text': str,
            'json': str,
            'password': str,
            'blob': str,
            'upload': str,
            'integer': long,
            'bigint': long,
            'float': float,
            'double': float,
            'date': datetime.date,
            'time': datetime.time,
            'datetime': datetime.datetime,
            'id': long,
            'reference': long,
            'list:string': list,
            'list:integer': list,
            'list:reference': list,
        }

    def file_exists(self, filename): pass
    def file_open(self, filename, mode='rb', lock=True): pass
    def file_close(self, fileobj): pass

    def expand(self,expression,field_type=None):
        if isinstance(expression,Field):
            if expression.type=='id':
                return "%s._id" % expression.tablename
        return BaseAdapter.expand(self,expression,field_type)

    def AND(self,first,second):
        return '(%s && %s)' % (self.expand(first),self.expand(second))

    def OR(self,first,second):
        return '(%s || %s)' % (self.expand(first),self.expand(second))

    def EQ(self,first,second):
        if second is None:
            return '(%s == null)' % self.expand(first)
        return '(%s == %s)' % (self.expand(first),self.expand(second,first.type))

    def NE(self,first,second):
        if second is None:
            return '(%s != null)' % self.expand(first)
        return '(%s != %s)' % (self.expand(first),self.expand(second,first.type))

    def COMMA(self,first,second):
        return '%s + %s' % (self.expand(first),self.expand(second))

    def represent(self, obj, fieldtype):
        value = NoSQLAdapter.represent(self, obj, fieldtype)
        if fieldtype == 'id':
            return repr(str(long(value)))
        elif fieldtype in ('date', 'time', 'datetime', 'boolean'):
            if self.db.has_serializer('json'):
                return self.db.serialize('json', value)
            else:
                return json.dumps(value)
            if not self._db.has_serializer('json'):
                raise ImportError("No json serializers available")
        return repr(not isinstance(value, unicode) and value
                    or value and value.encode('utf8'))

    def __init__(self,db,uri='couchdb://127.0.0.1:5984',
                 pool_size=0,folder=None,db_codec ='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):
        self.db = db
        self.uri = uri
        if do_connect: self.find_driver(adapter_args)
        self.dbengine = 'couchdb'
        self.folder = folder
        db['_lastsql'] = ''
        self.db_codec = 'UTF-8'
        self._after_connection = after_connection
        self.pool_size = pool_size

        url='http://'+uri[10:]
        def connector(url=url,driver_args=driver_args):
            return self.driver.Server(url,**driver_args)
        self.reconnect(connector,cursor=False)

    def create_table(self, table, migrate=True, fake_migrate=False, polymodel=None):
        if migrate:
            try:
                self.connection.create(table._tablename)
            except:
                pass

    def insert(self,table,fields):
        id = uuid2int(self.db.uuid())
        ctable = self.connection[table._tablename]
        values = dict((k.name,self.represent(v,k.type)) for k,v in fields)
        values['_id'] = str(id)
        ctable.save(values)
        return id

    def _select(self,query,fields,attributes):
        if not isinstance(query,Query):
            raise SyntaxError("Not Supported")
        for key in set(attributes.keys())-SELECT_ARGS:
            raise SyntaxError('invalid select attribute: %s' % key)
        new_fields=[]
        for item in fields:
            if isinstance(item,SQLALL):
                new_fields += item._table
            else:
                new_fields.append(item)
        def uid(fd):
            return fd=='id' and '_id' or fd
        def get(row,fd):
            return fd=='id' and long(row['_id']) or row.get(fd,None)
        fields = new_fields
        tablename = self.get_table(query)
        fieldnames = [f.name for f in (fields or self.db[tablename])]
        colnames = ['%s.%s' % (tablename,k) for k in fieldnames]
        fields = ','.join(['%s.%s' % (tablename,uid(f)) for f in fieldnames])
        fn="(function(%(t)s){if(%(query)s)emit(%(order)s,[%(fields)s]);})" %\
            dict(t=tablename,
                 query=self.expand(query),
                 order='%s._id' % tablename,
                 fields=fields)
        return fn, colnames

    def select(self,query,fields,attributes):
        if not isinstance(query,Query):
            raise SyntaxError("Not Supported")
        fn, colnames = self._select(query,fields,attributes)
        tablename = colnames[0].split('.')[0]
        ctable = self.connection[tablename]
        rows = [cols['value'] for cols in ctable.query(fn)]
        processor = attributes.get('processor',self.parse)
        return processor(rows,fields,colnames,False)

    def delete(self,tablename,query):
        if not isinstance(query,Query):
            raise SyntaxError("Not Supported")
        if query.first.type=='id' and query.op==self.EQ:
            id = query.second
            tablename = query.first.tablename
            assert(tablename == query.first.tablename)
            ctable = self.connection[tablename]
            try:
                del ctable[str(id)]
                return 1
            except couchdb.http.ResourceNotFound:
                return 0
        else:
            tablename = self.get_table(query)
            rows = self.select(query,[self.db[tablename]._id],{})
            ctable = self.connection[tablename]
            for row in rows:
                del ctable[str(row.id)]
            return len(rows)

    def update(self,tablename,query,fields):
        if not isinstance(query,Query):
            raise SyntaxError("Not Supported")
        if query.first.type=='id' and query.op==self.EQ:
            id = query.second
            tablename = query.first.tablename
            ctable = self.connection[tablename]
            try:
                doc = ctable[str(id)]
                for key,value in fields:
                    doc[key.name] = self.represent(value,self.db[tablename][key.name].type)
                ctable.save(doc)
                return 1
            except couchdb.http.ResourceNotFound:
                return 0
        else:
            tablename = self.get_table(query)
            rows = self.select(query,[self.db[tablename]._id],{})
            ctable = self.connection[tablename]
            table = self.db[tablename]
            for row in rows:
                doc = ctable[str(row.id)]
                for key,value in fields:
                    doc[key.name] = self.represent(value,table[key.name].type)
                ctable.save(doc)
            return len(rows)

    def count(self,query,distinct=None):
        if distinct:
            raise RuntimeError("COUNT DISTINCT not supported")
        if not isinstance(query,Query):
            raise SyntaxError("Not Supported")
        tablename = self.get_table(query)
        rows = self.select(query,[self.db[tablename]._id],{})
        return len(rows)
