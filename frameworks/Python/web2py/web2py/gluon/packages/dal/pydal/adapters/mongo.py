# -*- coding: utf-8 -*-
import datetime
import re

from .._globals import IDENTITY
from ..objects import Table, Query, Field, Expression
from ..helpers.classes import SQLALL
from ..helpers.methods import use_common_filters, xorify
from .base import NoSQLAdapter


class MongoDBAdapter(NoSQLAdapter):
    drivers = ('pymongo',)
    driver_auto_json = ['loads', 'dumps']

    uploads_in_blob = False

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

    error_messages = {"javascript_needed": "This must yet be replaced" +
                      " with javascript in order to work."}

    def __init__(self, db, uri='mongodb://127.0.0.1:5984/db',
                 pool_size=0, folder=None, db_codec='UTF-8',
                 credential_decoder=IDENTITY, driver_args={},
                 adapter_args={}, do_connect=True, after_connection=None):

        self.db = db
        self.uri = uri
        if do_connect: self.find_driver(adapter_args)
        import random
        from bson.objectid import ObjectId
        from bson.son import SON
        import pymongo.uri_parser

        m = pymongo.uri_parser.parse_uri(uri)

        self.SON = SON
        self.ObjectId = ObjectId
        self.random = random

        self.dbengine = 'mongodb'
        self.folder = folder
        db['_lastsql'] = ''
        self.db_codec = 'UTF-8'
        self._after_connection = after_connection
        self.pool_size = pool_size
        self.find_or_make_work_folder()
        #this is the minimum amount of replicates that it should wait
        # for on insert/update
        self.minimumreplication = adapter_args.get('minimumreplication', 0)
        # by default all inserts and selects are performand asynchronous,
        # but now the default is
        # synchronous, except when overruled by either this default or
        # function parameter
        self.safe = 1 if adapter_args.get('safe', True) else 0

        if isinstance(m, tuple):
            m = {"database": m[1]}
        if m.get('database') is None:
            raise SyntaxError("Database is required!")

        def connector(uri=self.uri, m=m):
            # Connection() is deprecated
            if hasattr(self.driver, "MongoClient"):
                Connection = self.driver.MongoClient
            else:
                Connection = self.driver.Connection
            return Connection(uri, w=self.safe)[m.get('database')]

        self.reconnect(connector, cursor=False)

    def object_id(self, arg=None):
        """ Convert input to a valid Mongodb ObjectId instance

        self.object_id("<random>") -> ObjectId (not unique) instance """
        if not arg:
            arg = 0
        if isinstance(arg, basestring):
            # we assume an integer as default input
            rawhex = len(arg.replace("0x", "").replace("L", "")) == 24
            if arg.isdigit() and (not rawhex):
                arg = int(arg)
            elif arg == "<random>":
                arg = int("0x%sL" % \
                "".join([self.random.choice("0123456789abcdef") \
                for x in range(24)]), 0)
            elif arg.isalnum():
                if not arg.startswith("0x"):
                    arg = "0x%s" % arg
                try:
                    arg = int(arg, 0)
                except ValueError, e:
                    raise ValueError(
                            "invalid objectid argument string: %s" % e)
            else:
                raise ValueError("Invalid objectid argument string. " +
                                 "Requires an integer or base 16 value")
        elif isinstance(arg, self.ObjectId):
            return arg

        if not isinstance(arg, (int, long)):
            raise TypeError("object_id argument must be of type " +
                            "ObjectId or an objectid representable integer")
        hexvalue = hex(arg)[2:].rstrip('L').zfill(24)
        return self.ObjectId(hexvalue)

    def parse_reference(self, value, field_type):
        # here we have to check for ObjectID before base parse
        if isinstance(value, self.ObjectId):
            value = long(str(value), 16)
        return super(MongoDBAdapter,
                     self).parse_reference(value, field_type)

    def parse_id(self, value, field_type):
        if isinstance(value, self.ObjectId):
            value = long(str(value), 16)
        return super(MongoDBAdapter,
                     self).parse_id(value, field_type)

    def represent(self, obj, fieldtype):
        # the base adatpter does not support MongoDB ObjectId
        if isinstance(obj, self.ObjectId):
            value = obj
        else:
            value = NoSQLAdapter.represent(self, obj, fieldtype)
        # reference types must be convert to ObjectID
        if fieldtype  =='date':
            if value is None:
                return value
            # this piece of data can be stripped off based on the fieldtype
            t = datetime.time(0, 0, 0)
            # mongodb doesn't has a date object and so it must datetime,
            # string or integer
            return datetime.datetime.combine(value, t)
        elif fieldtype == 'time':
            if value is None:
                return value
            # this piece of data can be stripped of based on the fieldtype
            d = datetime.date(2000, 1, 1)
            # mongodb doesn't has a  time object and so it must datetime,
            # string or integer
            return datetime.datetime.combine(d, value)
        elif fieldtype == "blob":
            if value is None:
                return value
            from bson import Binary
            if not isinstance(value, Binary):
                if not isinstance(value, basestring):
                    return Binary(str(value))
                return Binary(value)
            return value
        elif (isinstance(fieldtype, basestring) and
              fieldtype.startswith('list:')):
            if fieldtype.startswith('list:reference'):
                newval = []
                for v in value:
                    newval.append(self.object_id(v))
                return newval
            return value
        elif ((isinstance(fieldtype, basestring) and
               fieldtype.startswith("reference")) or
               (isinstance(fieldtype, Table)) or fieldtype=="id"):
            value = self.object_id(value)
        return value

    def create_table(self, table, migrate=True, fake_migrate=False,
                     polymodel=None, isCapped=False):
        if isCapped:
            raise RuntimeError("Not implemented")
        table._dbt = None

    def count(self, query, distinct=None, snapshot=True):
        if distinct:
            raise RuntimeError("COUNT DISTINCT not supported")
        if not isinstance(query,Query):
            raise SyntaxError("Not Supported")
        tablename = self.get_table(query)
        return long(self.select(query,[self.db[tablename]._id], {},
                                count=True,snapshot=snapshot)['count'])
        # Maybe it would be faster if we just implemented the pymongo
        # .count() function which is probably quicker?
        # therefor call __select() connection[table].find(query).count()
        # Since this will probably reduce the return set?

    def expand(self, expression, field_type=None):
        if isinstance(expression, Query):
            # any query using 'id':=
            # set name as _id (as per pymongo/mongodb primary key)
            # convert second arg to an objectid field
            # (if its not already)
            # if second arg is 0 convert to objectid
            if isinstance(expression.first,Field) and \
                    ((expression.first.type == 'id') or \
                    ("reference" in expression.first.type)):
                if expression.first.type == 'id':
                    expression.first.name = '_id'
                # cast to Mongo ObjectId
                if isinstance(expression.second, (tuple, list, set)):
                    expression.second = [self.object_id(item) for
                                         item in expression.second]
                else:
                    expression.second = self.object_id(expression.second)
                result = expression.op(expression.first, expression.second)

        if isinstance(expression, Field):
            if expression.type=='id':
                result = "_id"
            else:
                result =  expression.name
        elif isinstance(expression, (Expression, Query)):
            if not expression.second is None:
                result = expression.op(expression.first, expression.second)
            elif not expression.first is None:
                result = expression.op(expression.first)
            elif not isinstance(expression.op, str):
                result = expression.op()
            else:
                result = expression.op
        elif field_type:
            result = self.represent(expression,field_type)
        elif isinstance(expression,(list,tuple)):
            result = [self.represent(item,field_type) for
                      item in expression]
        else:
            result = expression
        return result

    def drop(self, table, mode=''):
        ctable = self.connection[table._tablename]
        ctable.drop()
        self._drop_cleanup(table)
        return

    def _get_safe(self, val=None):
        if val is None:
            return self.safe
        return 1 if val else 0

    def truncate(self, table, mode, safe=None):
        ctable = self.connection[table._tablename]
        ctable.remove(None, w=self._get_safe(safe))

    def select(self, query, fields, attributes, count=False,
               snapshot=False):
        mongofields_dict = self.SON()
        new_fields, mongosort_list = [], []
        # try an orderby attribute
        orderby = attributes.get('orderby', False)
        limitby = attributes.get('limitby', False)
        # distinct = attributes.get('distinct', False)
        if 'for_update' in attributes:
            self.db.logger.warn('mongodb does not support for_update')
        for key in set(attributes.keys())-set(('limitby', 'orderby',
                                               'for_update')):
            if attributes[key] is not None:
                self.db.logger.warn(
                    'select attribute not implemented: %s' % key)
        if limitby:
            limitby_skip, limitby_limit = limitby[0], int(limitby[1]) - 1
        else:
            limitby_skip = limitby_limit = 0
        if orderby:
            if isinstance(orderby, (list, tuple)):
                orderby = xorify(orderby)
            # !!!! need to add 'random'
            for f in self.expand(orderby).split(','):
                if f.startswith('-'):
                    mongosort_list.append((f[1:], -1))
                else:
                    mongosort_list.append((f, 1))
        for item in fields:
            if isinstance(item, SQLALL):
                new_fields += item._table
            else:
                new_fields.append(item)
        fields = new_fields
        if isinstance(query, Query):
            tablename = self.get_table(query)
        elif len(fields) != 0:
            tablename = fields[0].tablename
        else:
            raise SyntaxError("The table name could not be found in " +
                              "the query nor from the select statement.")

        if query:
            if use_common_filters(query):
                query = self.common_filter(query,[tablename])

        mongoqry_dict = self.expand(query)
        fields = fields or self.db[tablename]
        for field in fields:
            mongofields_dict[field.name] = 1
        ctable = self.connection[tablename]
        if count:
            return {'count': ctable.find(
                    mongoqry_dict, mongofields_dict,
                    skip=limitby_skip, limit=limitby_limit,
                    sort=mongosort_list, snapshot=snapshot).count()}
        else:
            # pymongo cursor object
            mongo_list_dicts = ctable.find(
                mongoqry_dict, mongofields_dict, skip=limitby_skip,
                limit=limitby_limit, sort=mongosort_list, snapshot=snapshot)
        rows = []
        # populate row in proper order
        # Here we replace ._id with .id to follow the standard naming
        colnames = []
        newnames = []
        for field in fields:
            colname = str(field)
            colnames.append(colname)
            tablename, fieldname = colname.split(".")
            if fieldname == "_id":
                # Mongodb reserved uuid key
                field.name = "id"
            newnames.append(".".join((tablename, field.name)))

        for record in mongo_list_dicts:
            row = []
            for colname in colnames:
                tablename, fieldname = colname.split(".")
                # switch to Mongo _id uuids for retrieving
                # record id's
                if fieldname == "id": fieldname = "_id"
                if fieldname in record:
                    value = record[fieldname]
                else:
                    value = None
                row.append(value)
            rows.append(row)
        processor = attributes.get('processor', self.parse)
        result = processor(rows, fields, newnames, False)
        return result

    def insert(self, table, fields, safe=None):
        """Safe determines whether a asynchronous request is done or a
        synchronous action is done
        For safety, we use by default synchronous requests"""

        values = dict()
        ctable = self.connection[table._tablename]
        for k, v in fields:
            if not k.name in ["id", "safe"]:
                fieldname = k.name
                fieldtype = table[k.name].type
                values[fieldname] = self.represent(v, fieldtype)

        ctable.insert(values, w=self._get_safe(safe))
        return long(str(values['_id']), 16)

    def update(self, tablename, query, fields, safe=None):
        # return amount of adjusted rows or zero, but no exceptions
        # @ related not finding the result
        if not isinstance(query, Query):
            raise RuntimeError("Not implemented")
        amount = self.count(query, False)
        if not isinstance(query, Query):
            raise SyntaxError("Not Supported")

        if query:
            if use_common_filters(query):
                query = self.common_filter(query,[tablename])

        filter = None
        if query:
            filter = self.expand(query)
        # do not try to update id fields to avoid backend errors
        modify = {'$set': dict((k.name, self.represent(v, k.type)) for
                  k, v in fields if (not k.name in ("_id", "id")))}
        try:
            result = self.connection[tablename].update(filter,
                       modify, multi=True, w=self._get_safe(safe))
            if safe:
                try:
                    # if result count is available fetch it
                    return result["n"]
                except (KeyError, AttributeError, TypeError):
                    return amount
            else:
                return amount
        except Exception, e:
            # TODO Reverse update query to verifiy that the query succeded
            raise RuntimeError("uncaught exception when updating rows: %s" % e)

    def delete(self, tablename, query, safe=None):
        amount = 0
        amount = self.count(query, False)
        if not isinstance(query, Query):
            raise RuntimeError("query type %s is not supported" % \
                               type(query))
        if query:
            if use_common_filters(query):
                query = self.common_filter(query,[tablename])
        filter = self.expand(query)
        self.connection[tablename].remove(filter, w=self._get_safe(safe))
        return amount

    def bulk_insert(self, table, items):
        return [self.insert(table,item) for item in items]

    ## OPERATORS
    def INVERT(self, first):
        #print "in invert first=%s" % first
        return '-%s' % self.expand(first)

    def NOT(self, first):
        op = self.expand(first)
        op_k = op.keys()[0]
        op_body = op[op_k]
        if type(op_body) is list:
            # apply De Morgan law for and/or
            # not(A and B) -> not(A) or not(B)
            # not(A or B)  -> not(A) and not(B)
            not_op = '$and' if op_k == '$or' else '$or'
            r = {not_op: [self.NOT(first.first), self.NOT(first.second)]}
        else:
            r = {op_k: {'$not': op_body}}
        return r

    def AND(self,first,second):
        # pymongo expects: .find({'$and': [{'x':'1'}, {'y':'2'}]})
        return {'$and': [self.expand(first),self.expand(second)]}

    def OR(self,first,second):
        # pymongo expects: .find({'$or': [{'name':'1'}, {'name':'2'}]})
        return {'$or': [self.expand(first),self.expand(second)]}

    def BELONGS(self, first, second):
        if isinstance(second, str):
            return {self.expand(first) : {"$in" : [ second[:-1]]} }
        elif second==[] or second==() or second==set():
            return {1:0}
        items = [self.expand(item, first.type) for item in second]
        return {self.expand(first) : {"$in" : items} }

    def EQ(self,first,second=None):
        result = {}
        result[self.expand(first)] = self.expand(second)
        return result

    def NE(self, first, second=None):
        result = {}
        result[self.expand(first)] = {'$ne': self.expand(second)}
        return result

    def LT(self,first,second=None):
        if second is None:
            raise RuntimeError("Cannot compare %s < None" % first)
        result = {}
        result[self.expand(first)] = {'$lt': self.expand(second)}
        return result

    def LE(self,first,second=None):
        if second is None:
            raise RuntimeError("Cannot compare %s <= None" % first)
        result = {}
        result[self.expand(first)] = {'$lte': self.expand(second)}
        return result

    def GT(self,first,second):
        result = {}
        result[self.expand(first)] = {'$gt': self.expand(second)}
        return result

    def GE(self,first,second=None):
        if second is None:
            raise RuntimeError("Cannot compare %s >= None" % first)
        result = {}
        result[self.expand(first)] = {'$gte': self.expand(second)}
        return result

    def ADD(self, first, second):
        raise NotImplementedError(self.error_messages["javascript_needed"])
        return '%s + %s' % (self.expand(first),
                            self.expand(second, first.type))

    def SUB(self, first, second):
        raise NotImplementedError(self.error_messages["javascript_needed"])
        return '(%s - %s)' % (self.expand(first),
                              self.expand(second, first.type))

    def MUL(self, first, second):
        raise NotImplementedError(self.error_messages["javascript_needed"])
        return '(%s * %s)' % (self.expand(first),
                              self.expand(second, first.type))

    def DIV(self, first, second):
        raise NotImplementedError(self.error_messages["javascript_needed"])
        return '(%s / %s)' % (self.expand(first),
                              self.expand(second, first.type))

    def MOD(self, first, second):
        raise NotImplementedError(self.error_messages["javascript_needed"])
        return '(%s %% %s)' % (self.expand(first),
                               self.expand(second, first.type))

    def AS(self, first, second):
        raise NotImplementedError(self.error_messages["javascript_needed"])
        return '%s AS %s' % (self.expand(first), second)

    # We could implement an option that simulates a full featured SQL
    # database. But I think the option should be set explicit or
    # implemented as another library.
    def ON(self, first, second):
        raise NotImplementedError("This is not possible in NoSQL" +
                                  " but can be simulated with a wrapper.")
        return '%s ON %s' % (self.expand(first), self.expand(second))

    # BLOW ARE TWO IMPLEMENTATIONS OF THE SAME FUNCITONS
    # WHICH ONE IS BEST?

    def COMMA(self, first, second):
        return '%s, %s' % (self.expand(first), self.expand(second))

    def LIKE(self, first, second):
        #escaping regex operators?
        return {self.expand(first): ('%s' % \
                self.expand(second, 'string').replace('%','/'))}

    def ILIKE(self, first, second):
        val = second if isinstance(second,self.ObjectId) else {
            '$regex': second.replace('%', ''), '$options': 'i'}
        return {self.expand(first): val}

    def STARTSWITH(self, first, second):
        #escaping regex operators?
        return {self.expand(first): ('/^%s/' % \
        self.expand(second, 'string'))}

    def ENDSWITH(self, first, second):
        #escaping regex operators?
        return {self.expand(first): ('/%s^/' % \
        self.expand(second, 'string'))}

    def CONTAINS(self, first, second, case_sensitive=False):
        # silently ignore, only case sensitive
        # There is a technical difference, but mongodb doesn't support
        # that, but the result will be the same
        val = second if isinstance(second,self.ObjectId) else \
            {'$regex':".*" + re.escape(self.expand(second, 'string')) + ".*"}
        return {self.expand(first) : val}

    def LIKE(self, first, second):
        import re
        return {self.expand(first): {'$regex': \
                re.escape(self.expand(second,
                                      'string')).replace('%','.*')}}

    #TODO verify full compatibilty with official SQL Like operator
    def STARTSWITH(self, first, second):
        #TODO  Solve almost the same problem as with endswith
        import re
        return {self.expand(first): {'$regex' : '^' +
                                     re.escape(self.expand(second,
                                                           'string'))}}

    #TODO verify full compatibilty with official SQL Like operator
    def ENDSWITH(self, first, second):
        #escaping regex operators?
        #TODO if searched for a name like zsa_corbitt and the function
        # is endswith('a') then this is also returned.
        # Aldo it end with a t
        import re
        return {self.expand(first): {'$regex': \
        re.escape(self.expand(second, 'string')) + '$'}}

    #TODO verify full compatibilty with official oracle contains operator
    def CONTAINS(self, first, second, case_sensitive=False):
        # silently ignore, only case sensitive
        #There is a technical difference, but mongodb doesn't support
        # that, but the result will be the same
        #TODO contains operators need to be transformed to Regex
        return {self.expand(first) : {'$regex': \
        ".*" + re.escape(self.expand(second, 'string')) + ".*"}}

