#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Basic caching classes and methods
---------------------------------

- Cache - The generic caching object interfacing with the others
- CacheInRam - providing caching in ram
- CacheOnDisk - provides caches on disk

Memcache is also available via a different module (see gluon.contrib.memcache)

When web2py is running on Google App Engine,
caching will be provided by the GAE memcache
(see gluon.contrib.gae_memcache)
"""
import time
import thread
import os
import gc
import sys
import logging
import re
import random
import hashlib
import datetime
import tempfile
from gluon import recfile
from gluon import portalocker
from collections import defaultdict
try:
    from collections import OrderedDict
except ImportError:
    from gluon.contrib.ordereddict import OrderedDict
try:
    from gluon import settings
    have_settings = True
except ImportError:
    have_settings = False

try:
   import cPickle as pickle
except:
   import pickle

try:
    import psutil
    HAVE_PSUTIL = True
except ImportError:
    HAVE_PSUTIL = False

def remove_oldest_entries(storage, percentage=90):
    # compute current memory usage (%)
    old_mem = psutil.virtual_memory().percent
    # if we have data in storage and utilization exceeds 90%
    while storage and old_mem > percentage:    
        # removed oldest entry
        storage.popitem(last=False)
        # garbage collect
        gc.collect(1)
        # comute used memory again
        new_mem = psutil.virtual_memory().percent
        # if the used memory did not decrease stop
        if new_mem >= old_mem: break
        # net new measurement for memory usage and loop
        old_mem = new_mem


logger = logging.getLogger("web2py.cache")

__all__ = ['Cache', 'lazy_cache']


DEFAULT_TIME_EXPIRE = 300

class CacheAbstract(object):
    """
    Abstract class for cache implementations.
    Main function just provides referenced api documentation.

    Use CacheInRam or CacheOnDisk instead which are derived from this class.

    Note:
        Michele says: there are signatures inside gdbm files that are used
        directly by the python gdbm adapter that often are lagging behind in the
        detection code in python part.
        On every occasion that a gdbm store is probed by the python adapter,
        the probe fails, because gdbm file version is newer.
        Using gdbm directly from C would work, because there is backward
        compatibility, but not from python!
        The .shelve file is discarded and a new one created (with new
        signature) and it works until it is probed again...
        The possible consequences are memory leaks and broken sessions.
    """

    cache_stats_name = 'web2py_cache_statistics'
    max_ram_utilization = 90 # percent

    def __init__(self, request=None):
        """Initializes the object

        Args:
            request: the global request object
        """
        raise NotImplementedError

    def __call__(self, key, f,
                 time_expire=DEFAULT_TIME_EXPIRE):
        """
        Tries to retrieve the value corresponding to `key` from the cache if the
        object exists and if it did not expire, else it calls the function `f`
        and stores the output in the cache corresponding to `key`. It always
        returns the function that is returned.

        Args:
            key(str): the key of the object to be stored or retrieved
            f(function): the function whose output is to be cached.

                If `f` is `None` the cache is cleared.
            time_expire(int): expiration of the cache in seconds.

                It's used to compare the current time with the time
                when the requested object was last saved in cache. It does not
                affect future requests. Setting `time_expire` to 0 or negative
                value forces the cache to refresh.
        """
        raise NotImplementedError

    def clear(self, regex=None):
        """
        Clears the cache of all keys that match the provided regular expression.
        If no regular expression is provided, it clears all entries in cache.

        Args:
            regex: if provided, only keys matching the regex will be cleared,
                otherwise all keys are cleared.
        """

        raise NotImplementedError

    def increment(self, key, value=1):
        """
        Increments the cached value for the given key by the amount in value

        Args:
            key(str): key for the cached object to be incremeneted
            value(int): amount of the increment (defaults to 1, can be negative)
        """
        raise NotImplementedError

    def _clear(self, storage, regex):
        """
        Auxiliary function called by `clear` to search and clear cache entries
        """
        r = re.compile(regex)
        for key in storage.keys():
            if r.match(str(key)):
                del storage[key]
        return


class CacheInRam(CacheAbstract):
    """
    Ram based caching

    This is implemented as global (per process, shared by all threads)
    dictionary.
    A mutex-lock mechanism avoid conflicts.
    """

    locker = thread.allocate_lock()
    meta_storage = {}
    stats = {}

    def __init__(self, request=None):
        self.initialized = False
        self.request = request
        self.storage = OrderedDict() if HAVE_PSUTIL else {}
        self.app = request.application if request else ''
    def initialize(self):
        if self.initialized:
            return
        else:
            self.initialized = True
        self.locker.acquire()
        if not self.app in self.meta_storage:
            self.storage = self.meta_storage[self.app] = \
                OrderedDict() if HAVE_PSUTIL else {}
            self.stats[self.app] = {'hit_total': 0, 'misses': 0}
        else:
            self.storage = self.meta_storage[self.app]
        self.locker.release()

    def clear(self, regex=None):
        self.initialize()
        self.locker.acquire()
        storage = self.storage
        if regex is None:
            storage.clear()
        else:
            self._clear(storage, regex)

        if not self.app in self.stats:
            self.stats[self.app] = {'hit_total': 0, 'misses': 0}

        self.locker.release()

    def __call__(self, key, f,
                 time_expire=DEFAULT_TIME_EXPIRE,
                 destroyer=None):
        """
        Attention! cache.ram does not copy the cached object.
        It just stores a reference to it. Turns out the deepcopying the object
        has some problems:

        - would break backward compatibility
        - would be limiting because people may want to cache live objects
        - would work unless we deepcopy no storage and retrival which would make
          things slow.

        Anyway. You can deepcopy explicitly in the function generating the value
        to be cached.
        """
        self.initialize()

        dt = time_expire
        now = time.time()

        self.locker.acquire()
        item = self.storage.get(key, None)
        if item and f is None:
            del self.storage[key]
            if destroyer:
                destroyer(item[1])
        self.stats[self.app]['hit_total'] += 1
        self.locker.release()

        if f is None:
            return None
        if item and (dt is None or item[0] > now - dt):
            return item[1]
        elif item and (item[0] < now - dt) and destroyer:
            destroyer(item[1])
        value = f()

        self.locker.acquire()
        self.storage[key] = (now, value)
        self.stats[self.app]['misses'] += 1
        if HAVE_PSUTIL and self.max_ram_utilization!=None and random.random()<0.10:
            remove_oldest_entries(self.storage, percentage = self.max_ram_utilization)
        self.locker.release()
        return value

    def increment(self, key, value=1):
        self.initialize()
        self.locker.acquire()
        try:
            if key in self.storage:
                value = self.storage[key][1] + value
            self.storage[key] = (time.time(), value)
        except BaseException, e:
            self.locker.release()
            raise e
        self.locker.release()
        return value


class CacheOnDisk(CacheAbstract):
    """
    Disk based cache

    This is implemented as a key value store where each key corresponds to a
    single file in disk which is replaced when the value changes.

    Disk cache provides persistance when web2py is started/stopped but it is
    slower than `CacheInRam`

    Values stored in disk cache must be pickable.
    """

    class PersistentStorage(object):
        """
        Implements a key based thread/process-safe safe storage in disk.
        """

        def __init__(self, folder, file_lock_time_wait=0.1):
            self.folder = folder
            self.key_filter_in = lambda key: key
            self.key_filter_out = lambda key: key
            self.file_lock_time_wait = file_lock_time_wait # How long we should wait before retrying to lock a file held by another process
            # We still need a mutex for each file as portalocker only blocks other processes
            self.file_locks = defaultdict(thread.allocate_lock)


            # Make sure we use valid filenames.
            if sys.platform == "win32":
                import base64
                def key_filter_in_windows(key):
                    """
                    Windows doesn't allow \ / : * ? "< > | in filenames.
                    To go around this encode the keys with base32.
                    """
                    return base64.b32encode(key)

                def key_filter_out_windows(key):
                    """
                    We need to decode the keys so regex based removal works.
                    """
                    return base64.b32decode(key)

                self.key_filter_in = key_filter_in_windows
                self.key_filter_out = key_filter_out_windows


        def wait_portalock(self, val_file):
            """
            Wait for the process file lock.
            """
            while True:
                try:
                    portalocker.lock(val_file, portalocker.LOCK_EX)
                    break
                except:
                    time.sleep(self.file_lock_time_wait)


        def acquire(self, key):
            self.file_locks[key].acquire()


        def release(self, key):
            self.file_locks[key].release()


        def __setitem__(self, key, value):
            key = self.key_filter_in(key)
            val_file = recfile.open(key, mode='wb', path=self.folder)
            self.wait_portalock(val_file)
            pickle.dump(value, val_file, pickle.HIGHEST_PROTOCOL)
            val_file.close()


        def __getitem__(self, key):
            key = self.key_filter_in(key)
            try:
                val_file = recfile.open(key, mode='rb', path=self.folder)
            except IOError:
                raise KeyError

            self.wait_portalock(val_file)
            value = pickle.load(recfile.open(key, 'rb', path=self.folder))
            val_file.close()
            return value


        def __contains__(self, key):
            key = self.key_filter_in(key)
            return (key in self.file_locks) or recfile.exists(key, path=self.folder)


        def __delitem__(self, key):
            key = self.key_filter_in(key)
            try:
                recfile.remove(key, path=self.folder)
            except IOError:
                raise KeyError


        def __iter__(self):
            for dirpath, dirnames, filenames in os.walk(self.folder):
                for filename in filenames:
                    yield self.key_filter_out(filename)


        def safe_apply(self, key, function, default_value=None):
            """ 
            Safely apply a function to the value of a key in storage and set
            the return value of the function to it.

            Return the result of applying the function.
            """
            key = self.key_filter_in(key)
            exists = True
            try:
                val_file = recfile.open(key, mode='r+b', path=self.folder)
            except IOError:
                exists = False
                val_file = recfile.open(key, mode='wb', path=self.folder)
            self.wait_portalock(val_file)
            if exists:
                timestamp, value = pickle.load(val_file)
            else:
                value = default_value
            new_value = function(value)
            val_file.seek(0)
            pickle.dump((time.time(), new_value), val_file, pickle.HIGHEST_PROTOCOL)
            val_file.truncate()
            val_file.close()
            return new_value


        def keys(self):
            return list(self.__iter__())


        def get(self, key, default=None):
            try:
                return self[key]
            except KeyError:
                return default


    def __init__(self, request=None, folder=None):
        self.initialized = False
        self.request = request
        self.folder = folder
        self.storage = None


    def initialize(self):
        if self.initialized:
            return
        else:
            self.initialized = True

        folder = self.folder
        request = self.request

        # Lets test if the cache folder exists, if not
        # we are going to create it
        folder = os.path.join(folder or request.folder, 'cache')

        if not os.path.exists(folder):
            os.mkdir(folder)

        self.storage = CacheOnDisk.PersistentStorage(folder)


    def __call__(self, key, f,
                 time_expire=DEFAULT_TIME_EXPIRE):
        self.initialize()

        def inc_hit_total(v):
            v['hit_total'] += 1
            return v

        def inc_misses(v):
            v['misses'] += 1
            return v

        dt = time_expire
        self.storage.acquire(key)
        self.storage.acquire(CacheAbstract.cache_stats_name)
        item = self.storage.get(key)
        self.storage.safe_apply(CacheAbstract.cache_stats_name, inc_hit_total,
                                default_value={'hit_total': 0, 'misses': 0})

        if item and f is None:
            del self.storage[key]

        if f is None:
            self.storage.release(CacheAbstract.cache_stats_name)
            self.storage.release(key)
            return None

        now = time.time()

        if item and ((dt is None) or (item[0] > now - dt)):
            value = item[1]
        else:
            value = f()
            self.storage[key] = (now, value)
            self.storage.safe_apply(CacheAbstract.cache_stats_name, inc_misses, 
                                    default_value={'hit_total': 0, 'misses': 0})

        self.storage.release(CacheAbstract.cache_stats_name)
        self.storage.release(key)
        return value


    def clear(self, regex=None):
        self.initialize()
        storage = self.storage
        if regex is None:
            keys = storage
        else:
            r = re.compile(regex)
            keys = (key for key in storage if r.match(key))
        for key in keys:
            storage.acquire(key)
            try:
                del storage[key]
            except KeyError:
                pass
            storage.release(key)


    def increment(self, key, value=1):
        self.initialize()
        self.storage.acquire(key)
        value = self.storage.safe_apply(key, lambda x: x + value, default_value=0)
        self.storage.release(key)
        return value



class CacheAction(object):
    def __init__(self, func, key, time_expire, cache, cache_model):
        self.__name__ = func.__name__
        self.__doc__ = func.__doc__
        self.func = func
        self.key = key
        self.time_expire = time_expire
        self.cache = cache
        self.cache_model = cache_model

    def __call__(self, *a, **b):
        if not self.key:
            key2 = self.__name__ + ':' + repr(a) + ':' + repr(b)
        else:
            key2 = self.key.replace('%(name)s', self.__name__)\
                .replace('%(args)s', str(a)).replace('%(vars)s', str(b))
        cache_model = self.cache_model
        if not cache_model or isinstance(cache_model, str):
            cache_model = getattr(self.cache, cache_model or 'ram')
        return cache_model(key2,
                           lambda a=a, b=b: self.func(*a, **b),
                           self.time_expire)


class Cache(object):
    """
    Sets up generic caching, creating an instance of both CacheInRam and
    CacheOnDisk.
    In case of GAE will make use of gluon.contrib.gae_memcache.

    - self.ram is an instance of CacheInRam
    - self.disk is an instance of CacheOnDisk
    """

    autokey = ':%(name)s:%(args)s:%(vars)s'

    def __init__(self, request):
        """
        Args:
            request: the global request object
        """
        # GAE will have a special caching
        if have_settings and settings.global_settings.web2py_runtime_gae:
            from gluon.contrib.gae_memcache import MemcacheClient
            self.ram = self.disk = MemcacheClient(request)
        else:
            # Otherwise use ram (and try also disk)
            self.ram = CacheInRam(request)
            try:
                self.disk = CacheOnDisk(request)
            except IOError:
                logger.warning('no cache.disk (IOError)')
            except AttributeError:
                # normally not expected anymore, as GAE has already
                # been accounted for
                logger.warning('no cache.disk (AttributeError)')

    def action(self, time_expire=DEFAULT_TIME_EXPIRE, cache_model=None,
             prefix=None, session=False, vars=True, lang=True,
             user_agent=False, public=True, valid_statuses=None,
             quick=None):
        """Better fit for caching an action

        Warning:
            Experimental!

        Currently only HTTP 1.1 compliant
        reference : http://code.google.com/p/doctype-mirror/wiki/ArticleHttpCaching

        Args:
            time_expire(int): same as @cache
            cache_model(str): same as @cache
            prefix(str): add a prefix to the calculated key
            session(bool): adds response.session_id to the key
            vars(bool): adds request.env.query_string
            lang(bool): adds T.accepted_language
            user_agent(bool or dict): if True, adds is_mobile and is_tablet to the key.
                Pass a dict to use all the needed values (uses str(.items()))
                (e.g. user_agent=request.user_agent()). Used only if session is
                not True
            public(bool): if False forces the Cache-Control to be 'private'
            valid_statuses: by default only status codes starting with 1,2,3 will be cached.
                pass an explicit list of statuses on which turn the cache on
            quick: Session,Vars,Lang,User-agent,Public:
                fast overrides with initials, e.g. 'SVLP' or 'VLP', or 'VLP'
        """
        from gluon import current
        from gluon.http import HTTP
        def wrap(func):
            def wrapped_f():
                if current.request.env.request_method != 'GET':
                    return func()
                if time_expire:
                    cache_control = 'max-age=%(time_expire)s, s-maxage=%(time_expire)s' % dict(time_expire=time_expire)
                    if quick:
                        session_ = True if 'S' in quick else False
                        vars_ = True if 'V' in quick else False
                        lang_ = True if 'L' in quick else False
                        user_agent_ = True if 'U' in quick else False
                        public_ = True if 'P' in quick else False
                    else:
                        session_, vars_, lang_, user_agent_, public_ = session, vars, lang, user_agent, public
                    if not session_ and public_:
                        cache_control += ', public'
                        expires = (current.request.utcnow + datetime.timedelta(seconds=time_expire)).strftime('%a, %d %b %Y %H:%M:%S GMT')
                    else:
                        cache_control += ', private'
                        expires = 'Fri, 01 Jan 1990 00:00:00 GMT'
                if cache_model:
                    #figure out the correct cache key
                    cache_key = [current.request.env.path_info, current.response.view]
                    if session_:
                        cache_key.append(current.response.session_id)
                    elif user_agent_:
                        if user_agent_ is True:
                            cache_key.append("%(is_mobile)s_%(is_tablet)s" % current.request.user_agent())
                        else:
                            cache_key.append(str(user_agent_.items()))
                    if vars_:
                        cache_key.append(current.request.env.query_string)
                    if lang_:
                        cache_key.append(current.T.accepted_language)
                    cache_key = hashlib.md5('__'.join(cache_key)).hexdigest()
                    if prefix:
                        cache_key = prefix + cache_key
                    try:
                        #action returns something
                        rtn = cache_model(cache_key, lambda : func(), time_expire=time_expire)
                        http, status = None, current.response.status
                    except HTTP, e:
                        #action raises HTTP (can still be valid)
                        rtn = cache_model(cache_key, lambda : e.body, time_expire=time_expire)
                        http, status = HTTP(e.status, rtn, **e.headers), e.status
                    else:
                        #action raised a generic exception
                        http = None
                else:
                    #no server-cache side involved
                    try:
                        #action returns something
                        rtn = func()
                        http, status = None, current.response.status
                    except HTTP, e:
                        #action raises HTTP (can still be valid)
                        status = e.status
                        http = HTTP(e.status, e.body, **e.headers)
                    else:
                        #action raised a generic exception
                        http = None
                send_headers = False
                if http and isinstance(valid_statuses, list):
                    if status in valid_statuses:
                        send_headers = True
                elif valid_statuses is None:
                    if str(status)[0] in '123':
                        send_headers = True
                if send_headers:
                    headers = {
                        'Pragma' : None,
                        'Expires' : expires,
                        'Cache-Control' : cache_control
                        }
                    current.response.headers.update(headers)
                if cache_model and not send_headers:
                    #we cached already the value, but the status is not valid
                    #so we need to delete the cached value
                    cache_model(cache_key, None)
                if http:
                    if send_headers:
                        http.headers.update(current.response.headers)
                    raise http
                return rtn
            wrapped_f.__name__ = func.__name__
            wrapped_f.__doc__ = func.__doc__
            return wrapped_f
        return wrap

    def __call__(self,
                 key=None,
                 time_expire=DEFAULT_TIME_EXPIRE,
                 cache_model=None):
        """
        Decorator function that can be used to cache any function/method.

        Args:
            key(str) : the key of the object to be store or retrieved
            time_expire(int) : expiration of the cache in seconds
                `time_expire` is used to compare the current time with the time
                when the requested object was last saved in cache.
                It does not affect future requests.
                Setting `time_expire` to 0 or negative value forces the cache to
                refresh.
            cache_model(str): can be "ram", "disk" or other (like "memcache").
                Defaults to "ram"

        When the function `f` is called, web2py tries to retrieve
        the value corresponding to `key` from the cache if the
        object exists and if it did not expire, else it calles the function `f`
        and stores the output in the cache corresponding to `key`. In the case
        the output of the function is returned.

        Example: ::

          @cache('key', 5000, cache.ram)
          def f():
              return time.ctime()

        Note:
            If the function `f` is an action, we suggest using
            @cache.action instead
        """

        def tmp(func, cache=self, cache_model=cache_model):
            return CacheAction(func, key, time_expire, self, cache_model)
        return tmp

    @staticmethod
    def with_prefix(cache_model, prefix):
        """
        allow replacing cache.ram with cache.with_prefix(cache.ram,'prefix')
        it will add prefix to all the cache keys used.
        """
        return lambda key, f, time_expire=DEFAULT_TIME_EXPIRE, prefix=prefix:\
            cache_model(prefix + key, f, time_expire)


def lazy_cache(key=None, time_expire=None, cache_model='ram'):
    """
    Can be used to cache any function including ones in modules,
    as long as the cached function is only called within a web2py request

    If a key is not provided, one is generated from the function name
    `time_expire` defaults to None (no cache expiration)

    If cache_model is "ram" then the model is current.cache.ram, etc.
    """
    def decorator(f, key=key, time_expire=time_expire, cache_model=cache_model):
        key = key or repr(f)

        def g(*c, **d):
            from gluon import current
            return current.cache(key, time_expire, cache_model)(f)(*c, **d)
        g.__name__ = f.__name__
        return g
    return decorator
