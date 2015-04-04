# -*- coding: utf-8 -*-
import copy
import marshal
import struct
import traceback

from .._compat import exists, copyreg


class Reference(long):

    def __allocate(self):
        if not self._record:
            self._record = self._table[long(self)]
        if not self._record:
            raise RuntimeError(
                "Using a recursive select but encountered a broken reference: %s %d"%(self._table, long(self)))

    def __getattr__(self, key):
        if key == 'id':
            return long(self)
        if key in self._table:
            self.__allocate()
        if self._record:
            return self._record.get(key,None) # to deal with case self.update_record()
        else:
            return None

    def get(self, key, default=None):
        return self.__getattr__(key, default)

    def __setattr__(self, key, value):
        if key.startswith('_'):
            long.__setattr__(self, key, value)
            return
        self.__allocate()
        self._record[key] =  value

    def __getitem__(self, key):
        if key == 'id':
            return long(self)
        self.__allocate()
        return self._record.get(key, None)

    def __setitem__(self,key,value):
        self.__allocate()
        self._record[key] = value

def Reference_unpickler(data):
    return marshal.loads(data)

def Reference_pickler(data):
    try:
        marshal_dump = marshal.dumps(long(data))
    except AttributeError:
        marshal_dump = 'i%s' % struct.pack('<i', long(data))
    return (Reference_unpickler, (marshal_dump,))

copyreg.pickle(Reference, Reference_pickler, Reference_unpickler)


class SQLCallableList(list):
    def __call__(self):
        return copy.copy(self)


class SQLALL(object):
    """
    Helper class providing a comma-separated string having all the field names
    (prefixed by table name and '.')

    normally only called from within gluon.dal
    """

    def __init__(self, table):
        self._table = table

    def __str__(self):
        return ', '.join([str(field) for field in self._table])


class SQLCustomType(object):
    """
    Allows defining of custom SQL types

    Args:
        type: the web2py type (default = 'string')
        native: the backend type
        encoder: how to encode the value to store it in the backend
        decoder: how to decode the value retrieved from the backend
        validator: what validators to use ( default = None, will use the
            default validator for type)

    Example::
        Define as:

            decimal = SQLCustomType(
                type ='double',
                native ='integer',
                encoder =(lambda x: int(float(x) * 100)),
                decoder = (lambda x: Decimal("0.00") + Decimal(str(float(x)/100)) )
                )

            db.define_table(
                'example',
                Field('value', type=decimal)
                )

    """

    def __init__(
        self,
        type='string',
        native=None,
        encoder=None,
        decoder=None,
        validator=None,
        _class=None,
        ):

        self.type = type
        self.native = native
        self.encoder = encoder or (lambda x: x)
        self.decoder = decoder or (lambda x: x)
        self.validator = validator
        self._class = _class or type

    def startswith(self, text=None):
        try:
            return self.type.startswith(self, text)
        except TypeError:
            return False

    def endswith(self, text=None):
        try:
            return self.type.endswith(self, text)
        except TypeError:
            return False

    def __getslice__(self, a=0, b=100):
        return None

    def __getitem__(self, i):
        return None

    def __str__(self):
        return self._class


class RecordUpdater(object):
    def __init__(self, colset, table, id):
        self.colset, self.db, self.tablename, self.id = \
            colset, table._db, table._tablename, id

    def __call__(self, **fields):
        colset, db, tablename, id = self.colset, self.db, self.tablename, self.id
        table = db[tablename]
        newfields = fields or dict(colset)
        for fieldname in newfields.keys():
            if not fieldname in table.fields or table[fieldname].type=='id':
                del newfields[fieldname]
        table._db(table._id==id,ignore_common_filters=True).update(**newfields)
        colset.update(newfields)
        return colset

class RecordDeleter(object):
    def __init__(self, table, id):
        self.db, self.tablename, self.id = table._db, table._tablename, id
    def __call__(self):
        return self.db(self.db[self.tablename]._id==self.id).delete()


class MethodAdder(object):
    def __init__(self,table):
        self.table = table
    def __call__(self):
        return self.register()
    def __getattr__(self,method_name):
        return self.register(method_name)
    def register(self,method_name=None):
        def _decorated(f):
            instance = self.table
            import types
            method = types.MethodType(f, instance, instance.__class__)
            name = method_name or f.func_name
            setattr(instance, name, method)
            return f
        return _decorated


class DatabaseStoredFile:

    web2py_filesystems = set()

    def escape(self,obj):
        return self.db._adapter.escape(obj)

    @staticmethod
    def try_create_web2py_filesystem(db):
        if not db._uri in DatabaseStoredFile.web2py_filesystems:
            if db._adapter.dbengine == 'mysql':
                sql = "CREATE TABLE IF NOT EXISTS web2py_filesystem (path VARCHAR(255), content LONGTEXT, PRIMARY KEY(path) ) ENGINE=InnoDB;"
            elif db._adapter.dbengine in ('postgres', 'sqlite'):
                sql = "CREATE TABLE IF NOT EXISTS web2py_filesystem (path VARCHAR(255), content TEXT, PRIMARY KEY(path));"
            db.executesql(sql)
            DatabaseStoredFile.web2py_filesystems.add(db._uri)

    def __init__(self,db,filename,mode):
        if not db._adapter.dbengine in ('mysql', 'postgres', 'sqlite'):
            raise RuntimeError("only MySQL/Postgres/SQLite can store metadata .table files in database for now")
        self.db = db
        self.filename = filename
        self.mode = mode
        DatabaseStoredFile.try_create_web2py_filesystem(db)
        self.p=0
        self.data = ''
        if mode in ('r','rw','a'):
            query = "SELECT content FROM web2py_filesystem WHERE path='%s'" \
                % filename
            rows = self.db.executesql(query)
            if rows:
                self.data = rows[0][0]
            elif exists(filename):
                datafile = open(filename, 'r')
                try:
                    self.data = datafile.read()
                finally:
                    datafile.close()
            elif mode in ('r','rw'):
                raise RuntimeError("File %s does not exist" % filename)

    def read(self, bytes):
        data = self.data[self.p:self.p+bytes]
        self.p += len(data)
        return data

    def readline(self):
        i = self.data.find('\n',self.p)+1
        if i>0:
            data, self.p = self.data[self.p:i], i
        else:
            data, self.p = self.data[self.p:], len(self.data)
        return data

    def write(self,data):
        self.data += data

    def close_connection(self):
        if self.db is not None:
            self.db.executesql(
                "DELETE FROM web2py_filesystem WHERE path='%s'" % self.filename)
            query = "INSERT INTO web2py_filesystem(path,content) VALUES ('%s','%s')"\
                % (self.filename, self.data.replace("'","''"))
            self.db.executesql(query)
            self.db.commit()
            self.db = None

    def close(self):
        self.close_connection()

    @staticmethod
    def exists(db, filename):
        if exists(filename):
            return True

        DatabaseStoredFile.try_create_web2py_filesystem(db)

        query = "SELECT path FROM web2py_filesystem WHERE path='%s'" % filename
        try:
            if db.executesql(query):
                return True
        except Exception, e:
            if not (db._adapter.isOperationalError(e) or
                    db._adapter.isProgrammingError(e)):
                raise
            # no web2py_filesystem found?
            tb = traceback.format_exc()
            db.logger.error("Could not retrieve %s\n%s" % (filename, tb))
        return False


class UseDatabaseStoredFile:

    def file_exists(self, filename):
        return DatabaseStoredFile.exists(self.db,filename)

    def file_open(self, filename, mode='rb', lock=True):
        return DatabaseStoredFile(self.db,filename,mode)

    def file_close(self, fileobj):
        fileobj.close_connection()

    def file_delete(self,filename):
        query = "DELETE FROM web2py_filesystem WHERE path='%s'" % filename
        self.db.executesql(query)
        self.db.commit()

