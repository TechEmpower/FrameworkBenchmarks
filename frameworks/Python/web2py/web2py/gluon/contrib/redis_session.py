"""
Developed by niphlod@gmail.com
"""

import redis
from gluon import current
from gluon.storage import Storage
import time
import logging
import thread

logger = logging.getLogger("web2py.session.redis")

locker = thread.allocate_lock()


def RedisSession(*args, **vars):
    """
    Usage example: put in models
    from gluon.contrib.redis_session import RedisSession
    sessiondb = RedisSession('localhost:6379',db=0, session_expiry=False, password=None)
    session.connect(request, response, db = sessiondb)

    Simple slip-in storage for session
    """

    locker.acquire()
    try:
        instance_name = 'redis_instance_' + current.request.application
        if not hasattr(RedisSession, instance_name):
            setattr(RedisSession, instance_name, RedisClient(*args, **vars))
        return getattr(RedisSession, instance_name)
    finally:
        locker.release()


class RedisClient(object):

    meta_storage = {}
    MAX_RETRIES = 5
    RETRIES = 0
    _release_script = None

    def __init__(self, server='localhost:6379', db=None, debug=False,
            session_expiry=False, with_lock=False, password=None):
        """session_expiry can be an integer, in seconds, to set the default expiration
           of sessions. The corresponding record will be deleted from the redis instance,
           and there's virtually no need to run sessions2trash.py
        """
        self.server = server
        self.password = password
        self.db = db or 0
        host, port = (self.server.split(':') + ['6379'])[:2]
        port = int(port)
        self.debug = debug
        if current and current.request:
            self.app = current.request.application
        else:
            self.app = ''
        self.r_server = redis.Redis(host=host, port=port, db=self.db, password=self.password)
        if with_lock:
            RedisClient._release_script = self.r_server.register_script(_LUA_RELEASE_LOCK)
        self.tablename = None
        self.session_expiry = session_expiry
        self.with_lock = with_lock

    def get(self, what, default):
        return self.tablename

    def Field(self, fieldname, type='string', length=None, default=None,
              required=False, requires=None):
        return None

    def define_table(self, tablename, *fields, **args):
        if not self.tablename:
            self.tablename = MockTable(
                self, self.r_server, tablename, self.session_expiry,
                self.with_lock)
        return self.tablename

    def __getitem__(self, key):
        return self.tablename

    def __call__(self, where=''):
        q = self.tablename.query
        return q

    def commit(self):
        # this is only called by session2trash.py
        pass


class MockTable(object):

    def __init__(self, db, r_server, tablename, session_expiry, with_lock=False):
        self.db = db
        self.r_server = r_server
        self.tablename = tablename
        # set the namespace for sessions of this app
        self.keyprefix = 'w2p:sess:%s' % tablename.replace(
            'web2py_session_', '')
        # fast auto-increment id (needed for session handling)
        self.serial = "%s:serial" % self.keyprefix
        # index of all the session keys of this app
        self.id_idx = "%s:id_idx" % self.keyprefix
        # remember the session_expiry setting
        self.session_expiry = session_expiry
        self.with_lock = with_lock

    def __call__(self, record_id, unique_key=None):
        # Support DAL shortcut query: table(record_id)

        # This will call the __getattr__ below
        # returning a MockQuery
        q = self.id

        # Instructs MockQuery, to behave as db(table.id == record_id)
        q.op = 'eq'
        q.value = record_id
        q.unique_key = unique_key

        row = q.select()
        return row[0] if row else Storage()

    def __getattr__(self, key):
        if key == 'id':
            # return a fake query. We need to query it just by id for normal operations
            self.query = MockQuery(
                field='id', db=self.r_server,
                prefix=self.keyprefix, session_expiry=self.session_expiry,
                with_lock=self.with_lock, unique_key=self.unique_key
            )
            return self.query
        elif key == '_db':
            # needed because of the calls in sessions2trash.py and globals.py
            return self.db

    def insert(self, **kwargs):
        # usually kwargs would be a Storage with several keys:
        # 'locked', 'client_ip','created_datetime','modified_datetime'
        # 'unique_key', 'session_data'
        # retrieve a new key
        newid = str(self.r_server.incr(self.serial))
        key = self.keyprefix + ':' + newid
        if self.with_lock:
            key_lock = key + ':lock'
            acquire_lock(self.r_server, key_lock, newid)
        with self.r_server.pipeline() as pipe:
            # add it to the index
            pipe.sadd(self.id_idx, key)
            # set a hash key with the Storage
            pipe.hmset(key, kwargs)
            if self.session_expiry:
                pipe.expire(key, self.session_expiry)
            pipe.execute()
        if self.with_lock:
            release_lock(self.r_server, key_lock, newid)
        return newid


class MockQuery(object):
    """a fake Query object that supports querying by id
       and listing all keys. No other operation is supported
    """
    def __init__(self, field=None, db=None, prefix=None, session_expiry=False,
                 with_lock=False, unique_key=None):
        self.field = field
        self.value = None
        self.db = db
        self.keyprefix = prefix
        self.op = None
        self.session_expiry = session_expiry
        self.with_lock = with_lock
        self.unique_key = unique_key

    def __eq__(self, value, op='eq'):
        self.value = value
        self.op = op

    def __gt__(self, value, op='ge'):
        self.value = value
        self.op = op

    def select(self):
        if self.op == 'eq' and self.field == 'id' and self.value:
            # means that someone wants to retrieve the key self.value
            key = self.keyprefix + ':' + str(self.value)
            if self.with_lock:
                acquire_lock(self.db, key + ':lock', self.value)
            rtn = self.db.hgetall(key)
            if rtn:
                if self.unique_key:
                    # make sure the id and unique_key are correct
                    if rtn['unique_key'] == self.unique_key:
                        rtn['update_record'] = self.update  # update record support
                    else:
                        rtn = None
            return [Storage(rtn)] if rtn else []
        elif self.op == 'ge' and self.field == 'id' and self.value == 0:
            # means that someone wants the complete list
            rtn = []
            id_idx = "%s:id_idx" % self.keyprefix
            # find all session keys of this app
            allkeys = self.db.smembers(id_idx)
            for sess in allkeys:
                val = self.db.hgetall(sess)
                if not val:
                    if self.session_expiry:
                        # clean up the idx, because the key expired
                        self.db.srem(id_idx, sess)
                    continue
                val = Storage(val)
                # add a delete_record method (necessary for sessions2trash.py)
                val.delete_record = RecordDeleter(
                    self.db, sess, self.keyprefix)
                rtn.append(val)
            return rtn
        else:
            raise Exception("Operation not supported")

    def update(self, **kwargs):
        # means that the session has been found and needs an update
        if self.op == 'eq' and self.field == 'id' and self.value:
            key = self.keyprefix + ':' + str(self.value)
            if not self.db.exists(key):
                return None
            with self.db.pipeline() as pipe:
                pipe.hmset(key, kwargs)
                if self.session_expiry:
                    pipe.expire(key, self.session_expiry)
                rtn = pipe.execute()[0]
            if self.with_lock:
                release_lock(self.db, key + ':lock', self.value)
            return rtn

    def delete(self, **kwargs):
        # means that we want this session to be deleted
        if self.op == 'eq' and self.field == 'id' and self.value:
            id_idx = "%s:id_idx" % self.keyprefix
            key = self.keyprefix + ':' + str(self.value)
            with self.db.pipeline() as pipe:
                pipe.delete(key)
                pipe.srem(id_idx, key)
                rtn = pipe.execute()
            return rtn[1]


class RecordDeleter(object):
    """Dumb record deleter to support sessions2trash.py"""

    def __init__(self, db, key, keyprefix):
        self.db, self.key, self.keyprefix = db, key, keyprefix

    def __call__(self):
        id_idx = "%s:id_idx" % self.keyprefix
        # remove from the index
        self.db.srem(id_idx, self.key)
        # remove the key itself
        self.db.delete(self.key)


def acquire_lock(conn, lockname, identifier, ltime=10):
    while True:
        if conn.set(lockname, identifier, ex=ltime, nx=True):
            return identifier
        time.sleep(.01)


_LUA_RELEASE_LOCK = """
if redis.call("get", KEYS[1]) == ARGV[1]
then
    return redis.call("del", KEYS[1])
else
    return 0
end
"""


def release_lock(conn, lockname, identifier):
    return RedisClient._release_script(
        keys=[lockname], args=[identifier],
        client=conn)
