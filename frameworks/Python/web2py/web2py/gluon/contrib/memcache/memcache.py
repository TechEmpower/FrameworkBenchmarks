#!/usr/bin/env python

"""client module for memcached (memory cache daemon)

Overview
========

See U{the MemCached homepage<http://www.danga.com/memcached>} for more
about memcached.

Usage summary
=============

This should give you a feel for how this module operates::

    import memcache
    mc = memcache.Client(['127.0.0.1:11211'], debug=0)

    mc.set("some_key", "Some value")
    value = mc.get("some_key")

    mc.set("another_key", 3)
    mc.delete("another_key")

    mc.set("key", "1") # note that the key used for incr/decr must be
                       # a string.
    mc.incr("key")
    mc.decr("key")

The standard way to use memcache with a database is like this:

    key = derive_key(obj)
    obj = mc.get(key)
    if not obj:
        obj = backend_api.get(...)
        mc.set(key, obj)

    # we now have obj, and future passes through this code
    # will use the object from the cache.

Detailed Documentation
======================

More detailed documentation is available in the L{Client} class.

"""

from __future__ import print_function

import binascii
import os
import pickle
import re
import socket
import sys
import threading
import time
import zlib

import six


def cmemcache_hash(key):
    return (
        (((binascii.crc32(key.encode('ascii')) & 0xffffffff)
          >> 16) & 0x7fff) or 1)
serverHashFunction = cmemcache_hash


def useOldServerHashFunction():
    """Use the old python-memcache server hash function."""
    global serverHashFunction
    serverHashFunction = binascii.crc32

try:
    from zlib import compress, decompress
    _supports_compress = True
except ImportError:
    _supports_compress = False
    # quickly define a decompress just in case we recv compressed data.

    def decompress(val):
        raise _Error(
            "Received compressed data but I don't support "
            "compression (import error)")

from io import BytesIO
try:
    unicode
except NameError:
    _has_unicode = False
else:
    _has_unicode = True

try:
    _str_cls = basestring
except NameError:
    _str_cls = str

valid_key_chars_re = re.compile('[\x21-\x7e\x80-\xff]+$')


#  Original author: Evan Martin of Danga Interactive
__author__ = "Sean Reifschneider <jafo-memcached@tummy.com>"
__version__ = "1.53"
__copyright__ = "Copyright (C) 2003 Danga Interactive"
#  http://en.wikipedia.org/wiki/Python_Software_Foundation_License
__license__ = "Python Software Foundation License"

SERVER_MAX_KEY_LENGTH = 250
# Storing values larger than 1MB requires recompiling memcached.  If
# you do, this value can be changed by doing
# "memcache.SERVER_MAX_VALUE_LENGTH = N" after importing this module.
SERVER_MAX_VALUE_LENGTH = 1024 * 1024


class _Error(Exception):
    pass


class _ConnectionDeadError(Exception):
    pass


_DEAD_RETRY = 30  # number of seconds before retrying a dead server.
_SOCKET_TIMEOUT = 3  # number of seconds before sockets timeout.


class Client(threading.local):
    """Object representing a pool of memcache servers.

    See L{memcache} for an overview.

    In all cases where a key is used, the key can be either:
        1. A simple hashable type (string, integer, etc.).
        2. A tuple of C{(hashvalue, key)}.  This is useful if you want
        to avoid making this module calculate a hash value.  You may
        prefer, for example, to keep all of a given user's objects on
        the same memcache server, so you could use the user's unique
        id as the hash value.


    @group Setup: __init__, set_servers, forget_dead_hosts,
    disconnect_all, debuglog
    @group Insertion: set, add, replace, set_multi
    @group Retrieval: get, get_multi
    @group Integers: incr, decr
    @group Removal: delete, delete_multi
    @sort: __init__, set_servers, forget_dead_hosts, disconnect_all,
           debuglog,\ set, set_multi, add, replace, get, get_multi,
           incr, decr, delete, delete_multi
    """
    _FLAG_PICKLE = 1 << 0
    _FLAG_INTEGER = 1 << 1
    _FLAG_LONG = 1 << 2
    _FLAG_COMPRESSED = 1 << 3

    _SERVER_RETRIES = 10  # how many times to try finding a free server.

    # exceptions for Client
    class MemcachedKeyError(Exception):
        pass

    class MemcachedKeyLengthError(MemcachedKeyError):
        pass

    class MemcachedKeyCharacterError(MemcachedKeyError):
        pass

    class MemcachedKeyNoneError(MemcachedKeyError):
        pass

    class MemcachedKeyTypeError(MemcachedKeyError):
        pass

    class MemcachedStringEncodingError(Exception):
        pass

    def __init__(self, servers, debug=0, pickleProtocol=0,
                 pickler=pickle.Pickler, unpickler=pickle.Unpickler,
                 pload=None, pid=None,
                 server_max_key_length=None, server_max_value_length=None,
                 dead_retry=_DEAD_RETRY, socket_timeout=_SOCKET_TIMEOUT,
                 cache_cas=False, flush_on_reconnect=0, check_keys=True):
        """Create a new Client object with the given list of servers.

        @param servers: C{servers} is passed to L{set_servers}.
        @param debug: whether to display error messages when a server
        can't be contacted.
        @param pickleProtocol: number to mandate protocol used by
        (c)Pickle.
        @param pickler: optional override of default Pickler to allow
        subclassing.
        @param unpickler: optional override of default Unpickler to
        allow subclassing.
        @param pload: optional persistent_load function to call on
        pickle loading.  Useful for cPickle since subclassing isn't
        allowed.
        @param pid: optional persistent_id function to call on pickle
        storing.  Useful for cPickle since subclassing isn't allowed.
        @param dead_retry: number of seconds before retrying a
        blacklisted server. Default to 30 s.
        @param socket_timeout: timeout in seconds for all calls to a
        server. Defaults to 3 seconds.
        @param cache_cas: (default False) If true, cas operations will
        be cached.  WARNING: This cache is not expired internally, if
        you have a long-running process you will need to expire it
        manually via client.reset_cas(), or the cache can grow
        unlimited.
        @param server_max_key_length: (default SERVER_MAX_KEY_LENGTH)
        Data that is larger than this will not be sent to the server.
        @param server_max_value_length: (default
        SERVER_MAX_VALUE_LENGTH) Data that is larger than this will
        not be sent to the server.
        @param flush_on_reconnect: optional flag which prevents a
        scenario that can cause stale data to be read: If there's more
        than one memcached server and the connection to one is
        interrupted, keys that mapped to that server will get
        reassigned to another. If the first server comes back, those
        keys will map to it again. If it still has its data, get()s
        can read stale data that was overwritten on another
        server. This flag is off by default for backwards
        compatibility.
        @param check_keys: (default True) If True, the key is checked
        to ensure it is the correct length and composed of the right
        characters.
        """
        super(Client, self).__init__()
        self.debug = debug
        self.dead_retry = dead_retry
        self.socket_timeout = socket_timeout
        self.flush_on_reconnect = flush_on_reconnect
        self.set_servers(servers)
        self.stats = {}
        self.cache_cas = cache_cas
        self.reset_cas()
        self.do_check_key = check_keys

        # Allow users to modify pickling/unpickling behavior
        self.pickleProtocol = pickleProtocol
        self.pickler = pickler
        self.unpickler = unpickler
        self.persistent_load = pload
        self.persistent_id = pid
        self.server_max_key_length = server_max_key_length
        if self.server_max_key_length is None:
            self.server_max_key_length = SERVER_MAX_KEY_LENGTH
        self.server_max_value_length = server_max_value_length
        if self.server_max_value_length is None:
            self.server_max_value_length = SERVER_MAX_VALUE_LENGTH

        #  figure out the pickler style
        file = BytesIO()
        try:
            pickler = self.pickler(file, protocol=self.pickleProtocol)
            self.picklerIsKeyword = True
        except TypeError:
            self.picklerIsKeyword = False

    def reset_cas(self):
        """Reset the cas cache.

        This is only used if the Client() object was created with
        "cache_cas=True".  If used, this cache does not expire
        internally, so it can grow unbounded if you do not clear it
        yourself.
        """
        self.cas_ids = {}

    def set_servers(self, servers):
        """Set the pool of servers used by this client.

        @param servers: an array of servers.
        Servers can be passed in two forms:
            1. Strings of the form C{"host:port"}, which implies a
            default weight of 1.
            2. Tuples of the form C{("host:port", weight)}, where
            C{weight} is an integer weight value.

        """
        self.servers = [_Host(s, self.debug, dead_retry=self.dead_retry,
                              socket_timeout=self.socket_timeout,
                              flush_on_reconnect=self.flush_on_reconnect)
                        for s in servers]
        self._init_buckets()

    def get_stats(self, stat_args=None):
        """Get statistics from each of the servers.

        @param stat_args: Additional arguments to pass to the memcache
            "stats" command.

        @return: A list of tuples ( server_identifier,
            stats_dictionary ).  The dictionary contains a number of
            name/value pairs specifying the name of the status field
            and the string value associated with it.  The values are
            not converted from strings.
        """
        data = []
        for s in self.servers:
            if not s.connect():
                continue
            if s.family == socket.AF_INET:
                name = '%s:%s (%s)' % (s.ip, s.port, s.weight)
            elif s.family == socket.AF_INET6:
                name = '[%s]:%s (%s)' % (s.ip, s.port, s.weight)
            else:
                name = 'unix:%s (%s)' % (s.address, s.weight)
            if not stat_args:
                s.send_cmd('stats')
            else:
                s.send_cmd('stats ' + stat_args)
            serverData = {}
            data.append((name, serverData))
            readline = s.readline
            while 1:
                line = readline()
                if not line or line.strip() == 'END':
                    break
                stats = line.split(' ', 2)
                serverData[stats[1]] = stats[2]

        return(data)

    def get_slabs(self):
        data = []
        for s in self.servers:
            if not s.connect():
                continue
            if s.family == socket.AF_INET:
                name = '%s:%s (%s)' % (s.ip, s.port, s.weight)
            elif s.family == socket.AF_INET6:
                name = '[%s]:%s (%s)' % (s.ip, s.port, s.weight)
            else:
                name = 'unix:%s (%s)' % (s.address, s.weight)
            serverData = {}
            data.append((name, serverData))
            s.send_cmd('stats items')
            readline = s.readline
            while 1:
                line = readline()
                if not line or line.strip() == 'END':
                    break
                item = line.split(' ', 2)
                # 0 = STAT, 1 = ITEM, 2 = Value
                slab = item[1].split(':', 2)
                # 0 = items, 1 = Slab #, 2 = Name
                if slab[1] not in serverData:
                    serverData[slab[1]] = {}
                serverData[slab[1]][slab[2]] = item[2]
        return data

    def flush_all(self):
        """Expire all data in memcache servers that are reachable."""
        for s in self.servers:
            if not s.connect():
                continue
            s.flush()

    def debuglog(self, str):
        if self.debug:
            sys.stderr.write("MemCached: %s\n" % str)

    def _statlog(self, func):
        if func not in self.stats:
            self.stats[func] = 1
        else:
            self.stats[func] += 1

    def forget_dead_hosts(self):
        """Reset every host in the pool to an "alive" state."""
        for s in self.servers:
            s.deaduntil = 0

    def _init_buckets(self):
        self.buckets = []
        for server in self.servers:
            for i in range(server.weight):
                self.buckets.append(server)

    def _get_server(self, key):
        if isinstance(key, tuple):
            serverhash, key = key
        else:
            serverhash = serverHashFunction(key)

        if not self.buckets:
            return None, None

        for i in range(Client._SERVER_RETRIES):
            server = self.buckets[serverhash % len(self.buckets)]
            if server.connect():
                # print("(using server %s)" % server,)
                return server, key
            serverhash = serverHashFunction(str(serverhash) + str(i))
        return None, None

    def disconnect_all(self):
        for s in self.servers:
            s.close_socket()

    def delete_multi(self, keys, time=0, key_prefix=''):
        """Delete multiple keys in the memcache doing just one query.

        >>> notset_keys = mc.set_multi({'a1' : 'val1', 'a2' : 'val2'})
        >>> mc.get_multi(['a1', 'a2']) == {'a1' : 'val1','a2' : 'val2'}
        1
        >>> mc.delete_multi(['key1', 'key2'])
        1
        >>> mc.get_multi(['key1', 'key2']) == {}
        1

        This method is recommended over iterated regular L{delete}s as
        it reduces total latency, since your app doesn't have to wait
        for each round-trip of L{delete} before sending the next one.

        @param keys: An iterable of keys to clear
        @param time: number of seconds any subsequent set / update
        commands should fail. Defaults to 0 for no delay.
        @param key_prefix: Optional string to prepend to each key when
            sending to memcache.  See docs for L{get_multi} and
            L{set_multi}.
        @return: 1 if no failure in communication with any memcacheds.
        @rtype: int
        """

        self._statlog('delete_multi')

        server_keys, prefixed_to_orig_key = self._map_and_prefix_keys(
            keys, key_prefix)

        # send out all requests on each server before reading anything
        dead_servers = []

        rc = 1
        for server in six.iterkeys(server_keys):
            bigcmd = []
            write = bigcmd.append
            if time is not None:
                for key in server_keys[server]:  # These are mangled keys
                    write("delete %s %d\r\n" % (key, time))
            else:
                for key in server_keys[server]:  # These are mangled keys
                    write("delete %s\r\n" % key)
            try:
                server.send_cmds(''.join(bigcmd))
            except socket.error as msg:
                rc = 0
                if isinstance(msg, tuple):
                    msg = msg[1]
                server.mark_dead(msg)
                dead_servers.append(server)

        # if any servers died on the way, don't expect them to respond.
        for server in dead_servers:
            del server_keys[server]

        for server, keys in six.iteritems(server_keys):
            try:
                for key in keys:
                    server.expect("DELETED")
            except socket.error as msg:
                if isinstance(msg, tuple):
                    msg = msg[1]
                server.mark_dead(msg)
                rc = 0
        return rc

    def delete(self, key, time=0):
        '''Deletes a key from the memcache.

        @return: Nonzero on success.
        @param time: number of seconds any subsequent set / update commands
        should fail. Defaults to None for no delay.
        @rtype: int
        '''
        return self._deletetouch(['DELETED', 'NOT_FOUND'], "delete", key, time)

    def touch(self, key, time=0):
        '''Updates the expiration time of a key in memcache.

        @return: Nonzero on success.
        @param time: Tells memcached the time which this value should
            expire, either as a delta number of seconds, or an absolute
            unix time-since-the-epoch value. See the memcached protocol
            docs section "Storage Commands" for more info on <exptime>. We
            default to 0 == cache forever.
        @rtype: int
        '''
        return self._deletetouch(['TOUCHED'], "touch", key, time)

    def _deletetouch(self, expected, cmd, key, time=0):
        if self.do_check_key:
            self.check_key(key)
        server, key = self._get_server(key)
        if not server:
            return 0
        self._statlog(cmd)
        if time is not None and time != 0:
            cmd = "%s %s %d" % (cmd, key, time)
        else:
            cmd = "%s %s" % (cmd, key)

        try:
            server.send_cmd(cmd)
            line = server.readline()
            if line and line.strip() in expected:
                return 1
            self.debuglog('%s expected %s, got: %r'
                          % (cmd, ' or '.join(expected), line))
        except socket.error as msg:
            if isinstance(msg, tuple):
                msg = msg[1]
            server.mark_dead(msg)
        return 0

    def incr(self, key, delta=1):
        """Increment value for C{key} by C{delta}

        Sends a command to the server to atomically increment the
        value for C{key} by C{delta}, or by 1 if C{delta} is
        unspecified.  Returns None if C{key} doesn't exist on server,
        otherwise it returns the new value after incrementing.

        Note that the value for C{key} must already exist in the
        memcache, and it must be the string representation of an
        integer.

        >>> mc.set("counter", "20")  # returns 1, indicating success
        1
        >>> mc.incr("counter")
        21
        >>> mc.incr("counter")
        22

        Overflow on server is not checked.  Be aware of values
        approaching 2**32.  See L{decr}.

        @param delta: Integer amount to increment by (should be zero
        or greater).

        @return: New value after incrementing.
        @rtype: int
        """
        return self._incrdecr("incr", key, delta)

    def decr(self, key, delta=1):
        """Decrement value for C{key} by C{delta}

        Like L{incr}, but decrements.  Unlike L{incr}, underflow is
        checked and new values are capped at 0.  If server value is 1,
        a decrement of 2 returns 0, not -1.

        @param delta: Integer amount to decrement by (should be zero
        or greater).

        @return: New value after decrementing or None on error.
        @rtype: int
        """
        return self._incrdecr("decr", key, delta)

    def _incrdecr(self, cmd, key, delta):
        if self.do_check_key:
            self.check_key(key)
        server, key = self._get_server(key)
        if not server:
            return None
        self._statlog(cmd)
        cmd = "%s %s %d" % (cmd, key, delta)
        try:
            server.send_cmd(cmd)
            line = server.readline()
            if line is None or line.strip() == 'NOT_FOUND':
                return None
            return int(line)
        except socket.error as msg:
            if isinstance(msg, tuple):
                msg = msg[1]
            server.mark_dead(msg)
            return None

    def add(self, key, val, time=0, min_compress_len=0):
        '''Add new key with value.

        Like L{set}, but only stores in memcache if the key doesn't
        already exist.

        @return: Nonzero on success.
        @rtype: int
        '''
        return self._set("add", key, val, time, min_compress_len)

    def append(self, key, val, time=0, min_compress_len=0):
        '''Append the value to the end of the existing key's value.

        Only stores in memcache if key already exists.
        Also see L{prepend}.

        @return: Nonzero on success.
        @rtype: int
        '''
        return self._set("append", key, val, time, min_compress_len)

    def prepend(self, key, val, time=0, min_compress_len=0):
        '''Prepend the value to the beginning of the existing key's value.

        Only stores in memcache if key already exists.
        Also see L{append}.

        @return: Nonzero on success.
        @rtype: int
        '''
        return self._set("prepend", key, val, time, min_compress_len)

    def replace(self, key, val, time=0, min_compress_len=0):
        '''Replace existing key with value.

        Like L{set}, but only stores in memcache if the key already exists.
        The opposite of L{add}.

        @return: Nonzero on success.
        @rtype: int
        '''
        return self._set("replace", key, val, time, min_compress_len)

    def set(self, key, val, time=0, min_compress_len=0):
        '''Unconditionally sets a key to a given value in the memcache.

        The C{key} can optionally be an tuple, with the first element
        being the server hash value and the second being the key.  If
        you want to avoid making this module calculate a hash value.
        You may prefer, for example, to keep all of a given user's
        objects on the same memcache server, so you could use the
        user's unique id as the hash value.

        @return: Nonzero on success.
        @rtype: int

        @param time: Tells memcached the time which this value should
        expire, either as a delta number of seconds, or an absolute
        unix time-since-the-epoch value. See the memcached protocol
        docs section "Storage Commands" for more info on <exptime>. We
        default to 0 == cache forever.

        @param min_compress_len: The threshold length to kick in
        auto-compression of the value using the zlib.compress()
        routine. If the value being cached is a string, then the
        length of the string is measured, else if the value is an
        object, then the length of the pickle result is measured. If
        the resulting attempt at compression yeilds a larger string
        than the input, then it is discarded. For backwards
        compatability, this parameter defaults to 0, indicating don't
        ever try to compress.

        '''
        return self._set("set", key, val, time, min_compress_len)

    def cas(self, key, val, time=0, min_compress_len=0):
        '''Check and set (CAS)

        Sets a key to a given value in the memcache if it hasn't been
        altered since last fetched. (See L{gets}).

        The C{key} can optionally be an tuple, with the first element
        being the server hash value and the second being the key.  If
        you want to avoid making this module calculate a hash value.
        You may prefer, for example, to keep all of a given user's
        objects on the same memcache server, so you could use the
        user's unique id as the hash value.

        @return: Nonzero on success.
        @rtype: int

        @param time: Tells memcached the time which this value should
        expire, either as a delta number of seconds, or an absolute
        unix time-since-the-epoch value. See the memcached protocol
        docs section "Storage Commands" for more info on <exptime>. We
        default to 0 == cache forever.

        @param min_compress_len: The threshold length to kick in
        auto-compression of the value using the zlib.compress()
        routine. If the value being cached is a string, then the
        length of the string is measured, else if the value is an
        object, then the length of the pickle result is measured. If
        the resulting attempt at compression yeilds a larger string
        than the input, then it is discarded. For backwards
        compatability, this parameter defaults to 0, indicating don't
        ever try to compress.
        '''
        return self._set("cas", key, val, time, min_compress_len)

    def _map_and_prefix_keys(self, key_iterable, key_prefix):
        """Compute the mapping of server (_Host instance) -> list of keys to
        stuff onto that server, as well as the mapping of prefixed key
        -> original key.
        """
        # Check it just once ...
        key_extra_len = len(key_prefix)
        if key_prefix and self.do_check_key:
            self.check_key(key_prefix)

        # server (_Host) -> list of unprefixed server keys in mapping
        server_keys = {}

        prefixed_to_orig_key = {}
        # build up a list for each server of all the keys we want.
        for orig_key in key_iterable:
            if isinstance(orig_key, tuple):
                # Tuple of hashvalue, key ala _get_server(). Caller is
                # essentially telling us what server to stuff this on.
                # Ensure call to _get_server gets a Tuple as well.
                str_orig_key = str(orig_key[1])

                # Gotta pre-mangle key before hashing to a
                # server. Returns the mangled key.
                server, key = self._get_server(
                    (orig_key[0], key_prefix + str_orig_key))
            else:
                # set_multi supports int / long keys.
                str_orig_key = str(orig_key)
                server, key = self._get_server(key_prefix + str_orig_key)

            # Now check to make sure key length is proper ...
            if self.do_check_key:
                self.check_key(str_orig_key, key_extra_len=key_extra_len)

            if not server:
                continue

            if server not in server_keys:
                server_keys[server] = []
            server_keys[server].append(key)
            prefixed_to_orig_key[key] = orig_key

        return (server_keys, prefixed_to_orig_key)

    def set_multi(self, mapping, time=0, key_prefix='', min_compress_len=0):
        '''Sets multiple keys in the memcache doing just one query.

        >>> notset_keys = mc.set_multi({'key1' : 'val1', 'key2' : 'val2'})
        >>> mc.get_multi(['key1', 'key2']) == {'key1' : 'val1',
        ...                                    'key2' : 'val2'}
        1


        This method is recommended over regular L{set} as it lowers
        the number of total packets flying around your network,
        reducing total latency, since your app doesn't have to wait
        for each round-trip of L{set} before sending the next one.

        @param mapping: A dict of key/value pairs to set.

        @param time: Tells memcached the time which this value should
            expire, either as a delta number of seconds, or an
            absolute unix time-since-the-epoch value. See the
            memcached protocol docs section "Storage Commands" for
            more info on <exptime>. We default to 0 == cache forever.

        @param key_prefix: Optional string to prepend to each key when
            sending to memcache. Allows you to efficiently stuff these
            keys into a pseudo-namespace in memcache:

            >>> notset_keys = mc.set_multi(
            ...     {'key1' : 'val1', 'key2' : 'val2'},
            ...     key_prefix='subspace_')
            >>> len(notset_keys) == 0
            True
            >>> mc.get_multi(['subspace_key1',
            ...               'subspace_key2']) == {'subspace_key1': 'val1',
            ...                                     'subspace_key2' : 'val2'}
            True

            Causes key 'subspace_key1' and 'subspace_key2' to be
            set. Useful in conjunction with a higher-level layer which
            applies namespaces to data in memcache.  In this case, the
            return result would be the list of notset original keys,
            prefix not applied.

        @param min_compress_len: The threshold length to kick in
            auto-compression of the value using the zlib.compress()
            routine. If the value being cached is a string, then the
            length of the string is measured, else if the value is an
            object, then the length of the pickle result is
            measured. If the resulting attempt at compression yeilds a
            larger string than the input, then it is discarded. For
            backwards compatability, this parameter defaults to 0,
            indicating don't ever try to compress.

        @return: List of keys which failed to be stored [ memcache out
           of memory, etc. ].

        @rtype: list
        '''
        self._statlog('set_multi')

        server_keys, prefixed_to_orig_key = self._map_and_prefix_keys(
            six.iterkeys(mapping), key_prefix)

        # send out all requests on each server before reading anything
        dead_servers = []
        notstored = []  # original keys.

        for server in six.iterkeys(server_keys):
            bigcmd = []
            write = bigcmd.append
            try:
                for key in server_keys[server]:  # These are mangled keys
                    store_info = self._val_to_store_info(
                        mapping[prefixed_to_orig_key[key]],
                        min_compress_len)
                    if store_info:
                        msg = "set %s %d %d %d\r\n%s\r\n"
                        write(msg % (key,
                                     store_info[0],
                                     time,
                                     store_info[1],
                                     store_info[2]))
                    else:
                        notstored.append(prefixed_to_orig_key[key])
                server.send_cmds(''.join(bigcmd))
            except socket.error as msg:
                if isinstance(msg, tuple):
                    msg = msg[1]
                server.mark_dead(msg)
                dead_servers.append(server)

        # if any servers died on the way, don't expect them to respond.
        for server in dead_servers:
            del server_keys[server]

        #  short-circuit if there are no servers, just return all keys
        if not server_keys:
            return(mapping.keys())

        for server, keys in six.iteritems(server_keys):
            try:
                for key in keys:
                    if server.readline() == 'STORED':
                        continue
                    else:
                        # un-mangle.
                        notstored.append(prefixed_to_orig_key[key])
            except (_Error, socket.error) as msg:
                if isinstance(msg, tuple):
                    msg = msg[1]
                server.mark_dead(msg)
        return notstored

    def _val_to_store_info(self, val, min_compress_len):
        """Transform val to a storable representation.

        Returns a tuple of the flags, the length of the new value, and
        the new value itself.
        """
        flags = 0
        if isinstance(val, str):
            pass
        elif isinstance(val, int):
            flags |= Client._FLAG_INTEGER
            val = "%d" % val
            # force no attempt to compress this silly string.
            min_compress_len = 0
        elif isinstance(val, long):
            flags |= Client._FLAG_LONG
            val = "%d" % val
            # force no attempt to compress this silly string.
            min_compress_len = 0
        else:
            flags |= Client._FLAG_PICKLE
            file = BytesIO()
            if self.picklerIsKeyword:
                pickler = self.pickler(file, protocol=self.pickleProtocol)
            else:
                pickler = self.pickler(file, self.pickleProtocol)
            if self.persistent_id:
                pickler.persistent_id = self.persistent_id
            pickler.dump(val)
            val = file.getvalue()

        lv = len(val)
        # We should try to compress if min_compress_len > 0 and we
        # could import zlib and this string is longer than our min
        # threshold.
        if min_compress_len and lv > min_compress_len:
            comp_val = zlib.compress(val)
            # Only retain the result if the compression result is smaller
            # than the original.
            if len(comp_val) < lv:
                flags |= Client._FLAG_COMPRESSED
                val = comp_val

        #  silently do not store if value length exceeds maximum
        if (self.server_max_value_length != 0 and
                len(val) > self.server_max_value_length):
            return(0)

        return (flags, len(val), val)

    def _set(self, cmd, key, val, time, min_compress_len=0):
        if self.do_check_key:
            self.check_key(key)
        server, key = self._get_server(key)
        if not server:
            return 0

        def _unsafe_set():
            self._statlog(cmd)

            store_info = self._val_to_store_info(val, min_compress_len)
            if not store_info:
                return(0)

            if cmd == 'cas':
                if key not in self.cas_ids:
                    return self._set('set', key, val, time, min_compress_len)
                fullcmd = "%s %s %d %d %d %d\r\n%s" % (
                    cmd, key, store_info[0], time, store_info[1],
                    self.cas_ids[key], store_info[2])
            else:
                fullcmd = "%s %s %d %d %d\r\n%s" % (
                    cmd, key, store_info[0],
                    time, store_info[1], store_info[2]
                )

            try:
                server.send_cmd(fullcmd)
                return(server.expect("STORED", raise_exception=True)
                       == "STORED")
            except socket.error as msg:
                if isinstance(msg, tuple):
                    msg = msg[1]
                server.mark_dead(msg)
            return 0

        try:
            return _unsafe_set()
        except _ConnectionDeadError:
            # retry once
            try:
                if server._get_socket():
                    return _unsafe_set()
            except (_ConnectionDeadError, socket.error) as msg:
                server.mark_dead(msg)
            return 0

    def _get(self, cmd, key):
        if self.do_check_key:
            self.check_key(key)
        server, key = self._get_server(key)
        if not server:
            return None

        def _unsafe_get():
            self._statlog(cmd)

            try:
                server.send_cmd("%s %s" % (cmd, key))
                rkey = flags = rlen = cas_id = None

                if cmd == 'gets':
                    rkey, flags, rlen, cas_id, = self._expect_cas_value(
                        server, raise_exception=True
                    )
                    if rkey and self.cache_cas:
                        self.cas_ids[rkey] = cas_id
                else:
                    rkey, flags, rlen, = self._expectvalue(
                        server, raise_exception=True
                    )

                if not rkey:
                    return None
                try:
                    value = self._recv_value(server, flags, rlen)
                finally:
                    server.expect("END", raise_exception=True)
            except (_Error, socket.error) as msg:
                if isinstance(msg, tuple):
                    msg = msg[1]
                server.mark_dead(msg)
                return None

            return value

        try:
            return _unsafe_get()
        except _ConnectionDeadError:
            # retry once
            try:
                if server.connect():
                    return _unsafe_get()
                return None
            except (_ConnectionDeadError, socket.error) as msg:
                server.mark_dead(msg)
            return None

    def get(self, key):
        '''Retrieves a key from the memcache.

        @return: The value or None.
        '''
        return self._get('get', key)

    def gets(self, key):
        '''Retrieves a key from the memcache. Used in conjunction with 'cas'.

        @return: The value or None.
        '''
        return self._get('gets', key)

    def get_multi(self, keys, key_prefix=''):
        '''Retrieves multiple keys from the memcache doing just one query.

        >>> success = mc.set("foo", "bar")
        >>> success = mc.set("baz", 42)
        >>> mc.get_multi(["foo", "baz", "foobar"]) == {
        ...     "foo": "bar", "baz": 42
        ... }
        1
        >>> mc.set_multi({'k1' : 1, 'k2' : 2}, key_prefix='pfx_') == []
        1

        This looks up keys 'pfx_k1', 'pfx_k2', ... . Returned dict
        will just have unprefixed keys 'k1', 'k2'.

        >>> mc.get_multi(['k1', 'k2', 'nonexist'],
        ...              key_prefix='pfx_') == {'k1' : 1, 'k2' : 2}
        1

        get_mult [ and L{set_multi} ] can take str()-ables like ints /
        longs as keys too. Such as your db pri key fields.  They're
        rotored through str() before being passed off to memcache,
        with or without the use of a key_prefix.  In this mode, the
        key_prefix could be a table name, and the key itself a db
        primary key number.

        >>> mc.set_multi({42: 'douglass adams',
        ...               46: 'and 2 just ahead of me'},
        ...              key_prefix='numkeys_') == []
        1
        >>> mc.get_multi([46, 42], key_prefix='numkeys_') == {
        ...     42: 'douglass adams',
        ...     46: 'and 2 just ahead of me'
        ... }
        1

        This method is recommended over regular L{get} as it lowers
        the number of total packets flying around your network,
        reducing total latency, since your app doesn't have to wait
        for each round-trip of L{get} before sending the next one.

        See also L{set_multi}.

        @param keys: An array of keys.

        @param key_prefix: A string to prefix each key when we
        communicate with memcache.  Facilitates pseudo-namespaces
        within memcache. Returned dictionary keys will not have this
        prefix.

        @return: A dictionary of key/value pairs that were
        available. If key_prefix was provided, the keys in the retured
        dictionary will not have it present.
        '''

        self._statlog('get_multi')

        server_keys, prefixed_to_orig_key = self._map_and_prefix_keys(
            keys, key_prefix)

        # send out all requests on each server before reading anything
        dead_servers = []
        for server in six.iterkeys(server_keys):
            try:
                server.send_cmd("get %s" % " ".join(server_keys[server]))
            except socket.error as msg:
                if isinstance(msg, tuple):
                    msg = msg[1]
                server.mark_dead(msg)
                dead_servers.append(server)

        # if any servers died on the way, don't expect them to respond.
        for server in dead_servers:
            del server_keys[server]

        retvals = {}
        for server in six.iterkeys(server_keys):
            try:
                line = server.readline()
                while line and line != 'END':
                    rkey, flags, rlen = self._expectvalue(server, line)
                    #  Bo Yang reports that this can sometimes be None
                    if rkey is not None:
                        val = self._recv_value(server, flags, rlen)
                        # un-prefix returned key.
                        retvals[prefixed_to_orig_key[rkey]] = val
                    line = server.readline()
            except (_Error, socket.error) as msg:
                if isinstance(msg, tuple):
                    msg = msg[1]
                server.mark_dead(msg)
        return retvals

    def _expect_cas_value(self, server, line=None, raise_exception=False):
        if not line:
            line = server.readline(raise_exception)

        if line and line[:5] == 'VALUE':
            resp, rkey, flags, len, cas_id = line.split()
            return (rkey, int(flags), int(len), int(cas_id))
        else:
            return (None, None, None, None)

    def _expectvalue(self, server, line=None, raise_exception=False):
        if not line:
            line = server.readline(raise_exception)

        if line and line[:5] == 'VALUE':
            resp, rkey, flags, len = line.split()
            flags = int(flags)
            rlen = int(len)
            return (rkey, flags, rlen)
        else:
            return (None, None, None)

    def _recv_value(self, server, flags, rlen):
        rlen += 2  # include \r\n
        buf = server.recv(rlen)
        if len(buf) != rlen:
            raise _Error("received %d bytes when expecting %d"
                         % (len(buf), rlen))

        if len(buf) == rlen:
            buf = buf[:-2]  # strip \r\n

        if flags & Client._FLAG_COMPRESSED:
            buf = zlib.decompress(buf)

        if flags == 0 or flags == Client._FLAG_COMPRESSED:
            # Either a bare string or a compressed string now decompressed...
            val = buf
        elif flags & Client._FLAG_INTEGER:
            val = int(buf)
        elif flags & Client._FLAG_LONG:
            val = long(buf)
        elif flags & Client._FLAG_PICKLE:
            try:
                file = BytesIO(buf)
                unpickler = self.unpickler(file)
                if self.persistent_load:
                    unpickler.persistent_load = self.persistent_load
                val = unpickler.load()
            except Exception as e:
                self.debuglog('Pickle error: %s\n' % e)
                return None
        else:
            self.debuglog("unknown flags on get: %x\n" % flags)
            raise ValueError('Unknown flags on get: %x' % flags)

        return val

    def check_key(self, key, key_extra_len=0):
        """Checks sanity of key.

            Fails if:

            Key length is > SERVER_MAX_KEY_LENGTH (Raises MemcachedKeyLength).
            Contains control characters  (Raises MemcachedKeyCharacterError).
            Is not a string (Raises MemcachedStringEncodingError)
            Is an unicode string (Raises MemcachedStringEncodingError)
            Is not a string (Raises MemcachedKeyError)
            Is None (Raises MemcachedKeyError)
        """
        if isinstance(key, tuple):
            key = key[1]
        if not key:
            raise Client.MemcachedKeyNoneError("Key is None")

        # Make sure we're not a specific unicode type, if we're old enough that
        # it's a separate type.
        if _has_unicode is True and isinstance(key, unicode):
            raise Client.MemcachedStringEncodingError(
                "Keys must be str()'s, not unicode.  Convert your unicode "
                "strings using mystring.encode(charset)!")
        if not isinstance(key, str):
            raise Client.MemcachedKeyTypeError("Key must be str()'s")

        if isinstance(key, _str_cls):
            if (self.server_max_key_length != 0 and
                    len(key) + key_extra_len > self.server_max_key_length):
                raise Client.MemcachedKeyLengthError(
                    "Key length is > %s" % self.server_max_key_length
                )
            if not valid_key_chars_re.match(key):
                raise Client.MemcachedKeyCharacterError(
                    "Control characters not allowed")


class _Host(object):

    def __init__(self, host, debug=0, dead_retry=_DEAD_RETRY,
                 socket_timeout=_SOCKET_TIMEOUT, flush_on_reconnect=0):
        self.dead_retry = dead_retry
        self.socket_timeout = socket_timeout
        self.debug = debug
        self.flush_on_reconnect = flush_on_reconnect
        if isinstance(host, tuple):
            host, self.weight = host
        else:
            self.weight = 1

        #  parse the connection string
        m = re.match(r'^(?P<proto>unix):(?P<path>.*)$', host)
        if not m:
            m = re.match(r'^(?P<proto>inet6):'
                         r'\[(?P<host>[^\[\]]+)\](:(?P<port>[0-9]+))?$', host)
        if not m:
            m = re.match(r'^(?P<proto>inet):'
                         r'(?P<host>[^:]+)(:(?P<port>[0-9]+))?$', host)
        if not m:
            m = re.match(r'^(?P<host>[^:]+)(:(?P<port>[0-9]+))?$', host)
        if not m:
            raise ValueError('Unable to parse connection string: "%s"' % host)

        hostData = m.groupdict()
        if hostData.get('proto') == 'unix':
            self.family = socket.AF_UNIX
            self.address = hostData['path']
        elif hostData.get('proto') == 'inet6':
            self.family = socket.AF_INET6
            self.ip = hostData['host']
            self.port = int(hostData.get('port') or 11211)
            self.address = (self.ip, self.port)
        else:
            self.family = socket.AF_INET
            self.ip = hostData['host']
            self.port = int(hostData.get('port') or 11211)
            self.address = (self.ip, self.port)

        self.deaduntil = 0
        self.socket = None
        self.flush_on_next_connect = 0

        self.buffer = ''

    def debuglog(self, str):
        if self.debug:
            sys.stderr.write("MemCached: %s\n" % str)

    def _check_dead(self):
        if self.deaduntil and self.deaduntil > time.time():
            return 1
        self.deaduntil = 0
        return 0

    def connect(self):
        if self._get_socket():
            return 1
        return 0

    def mark_dead(self, reason):
        self.debuglog("MemCache: %s: %s.  Marking dead." % (self, reason))
        self.deaduntil = time.time() + self.dead_retry
        if self.flush_on_reconnect:
            self.flush_on_next_connect = 1
        self.close_socket()

    def _get_socket(self):
        if self._check_dead():
            return None
        if self.socket:
            return self.socket
        s = socket.socket(self.family, socket.SOCK_STREAM)
        if hasattr(s, 'settimeout'):
            s.settimeout(self.socket_timeout)
        try:
            s.connect(self.address)
        except socket.timeout as msg:
            self.mark_dead("connect: %s" % msg)
            return None
        except socket.error as msg:
            if isinstance(msg, tuple):
                msg = msg[1]
            self.mark_dead("connect: %s" % msg)
            return None
        self.socket = s
        self.buffer = ''
        if self.flush_on_next_connect:
            self.flush()
            self.flush_on_next_connect = 0
        return s

    def close_socket(self):
        if self.socket:
            self.socket.close()
            self.socket = None

    def send_cmd(self, cmd):
        self.socket.sendall(cmd + '\r\n')

    def send_cmds(self, cmds):
        """cmds already has trailing \r\n's applied."""
        self.socket.sendall(cmds)

    def readline(self, raise_exception=False):
        """Read a line and return it.

        If "raise_exception" is set, raise _ConnectionDeadError if the
        read fails, otherwise return an empty string.
        """
        buf = self.buffer
        if self.socket:
            recv = self.socket.recv
        else:
            recv = lambda bufsize: ''

        while True:
            index = buf.find('\r\n')
            if index >= 0:
                break
            data = recv(4096)
            if not data:
                # connection close, let's kill it and raise
                self.mark_dead('connection closed in readline()')
                if raise_exception:
                    raise _ConnectionDeadError()
                else:
                    return ''

            buf += data
        self.buffer = buf[index + 2:]
        return buf[:index]

    def expect(self, text, raise_exception=False):
        line = self.readline(raise_exception)
        if line != text:
            self.debuglog("while expecting '%s', got unexpected response '%s'"
                          % (text, line))
        return line

    def recv(self, rlen):
        self_socket_recv = self.socket.recv
        buf = self.buffer
        while len(buf) < rlen:
            foo = self_socket_recv(max(rlen - len(buf), 4096))
            buf += foo
            if not foo:
                raise _Error('Read %d bytes, expecting %d, '
                             'read returned 0 length bytes' % (len(buf), rlen))
        self.buffer = buf[rlen:]
        return buf[:rlen]

    def flush(self):
        self.send_cmd('flush_all')
        self.expect('OK')

    def __str__(self):
        d = ''
        if self.deaduntil:
            d = " (dead until %d)" % self.deaduntil

        if self.family == socket.AF_INET:
            return "inet:%s:%d%s" % (self.address[0], self.address[1], d)
        elif self.family == socket.AF_INET6:
            return "inet6:[%s]:%d%s" % (self.address[0], self.address[1], d)
        else:
            return "unix:%s%s" % (self.address, d)


def _doctest():
    import doctest
    import memcache
    servers = ["127.0.0.1:11211"]
    mc = Client(servers, debug=1)
    globs = {"mc": mc}
    return doctest.testmod(memcache, globs=globs)

if __name__ == "__main__":
    failures = 0
    print("Testing docstrings...")
    _doctest()
    print("Running tests:")
    print()
    serverList = [["127.0.0.1:11211"]]
    if '--do-unix' in sys.argv:
        serverList.append([os.path.join(os.getcwd(), 'memcached.socket')])

    for servers in serverList:
        mc = Client(servers, debug=1)

        def to_s(val):
            if not isinstance(val, _str_cls):
                return "%s (%s)" % (val, type(val))
            return "%s" % val

        def test_setget(key, val):
            global failures
            print("Testing set/get {'%s': %s} ..."
                  % (to_s(key), to_s(val)), end=" ")
            mc.set(key, val)
            newval = mc.get(key)
            if newval == val:
                print("OK")
                return 1
            else:
                print("FAIL")
                failures += 1
                return 0

        class FooStruct(object):

            def __init__(self):
                self.bar = "baz"

            def __str__(self):
                return "A FooStruct"

            def __eq__(self, other):
                if isinstance(other, FooStruct):
                    return self.bar == other.bar
                return 0

        test_setget("a_string", "some random string")
        test_setget("an_integer", 42)
        if test_setget("long", long(1 << 30)):
            print("Testing delete ...", end=" ")
            if mc.delete("long"):
                print("OK")
            else:
                print("FAIL")
                failures += 1
            print("Checking results of delete ...", end=" ")
            if mc.get("long") is None:
                print("OK")
            else:
                print("FAIL")
                failures += 1
        print("Testing get_multi ...",)
        print(mc.get_multi(["a_string", "an_integer"]))

        #  removed from the protocol
        # if test_setget("timed_delete", 'foo'):
        #     print "Testing timed delete ...",
        #     if mc.delete("timed_delete", 1):
        #         print("OK")
        #     else:
        #         print("FAIL")
        #         failures += 1
        #     print "Checking results of timed delete ..."
        #     if mc.get("timed_delete") is None:
        #         print("OK")
        #     else:
        #         print("FAIL")
        #         failures += 1

        print("Testing get(unknown value) ...", end=" ")
        print(to_s(mc.get("unknown_value")))

        f = FooStruct()
        test_setget("foostruct", f)

        print("Testing incr ...", end=" ")
        x = mc.incr("an_integer", 1)
        if x == 43:
            print("OK")
        else:
            print("FAIL")
            failures += 1

        print("Testing decr ...", end=" ")
        x = mc.decr("an_integer", 1)
        if x == 42:
            print("OK")
        else:
            print("FAIL")
            failures += 1
        sys.stdout.flush()

        # sanity tests
        print("Testing sending spaces...", end=" ")
        sys.stdout.flush()
        try:
            x = mc.set("this has spaces", 1)
        except Client.MemcachedKeyCharacterError as msg:
            print("OK")
        else:
            print("FAIL")
            failures += 1

        print("Testing sending control characters...", end=" ")
        try:
            x = mc.set("this\x10has\x11control characters\x02", 1)
        except Client.MemcachedKeyCharacterError as msg:
            print("OK")
        else:
            print("FAIL")
            failures += 1

        print("Testing using insanely long key...", end=" ")
        try:
            x = mc.set('a'*SERVER_MAX_KEY_LENGTH, 1)
        except Client.MemcachedKeyLengthError as msg:
            print("FAIL")
            failures += 1
        else:
            print("OK")
        try:
            x = mc.set('a'*SERVER_MAX_KEY_LENGTH + 'a', 1)
        except Client.MemcachedKeyLengthError as msg:
            print("OK")
        else:
            print("FAIL")
            failures += 1

        print("Testing sending a unicode-string key...", end=" ")
        try:
            x = mc.set(unicode('keyhere'), 1)
        except Client.MemcachedStringEncodingError as msg:
            print("OK", end=" ")
        else:
            print("FAIL", end=" ")
            failures += 1
        try:
            x = mc.set((unicode('a')*SERVER_MAX_KEY_LENGTH).encode('utf-8'), 1)
        except Client.MemcachedKeyError:
            print("FAIL", end=" ")
            failures += 1
        else:
            print("OK", end=" ")
        s = pickle.loads('V\\u4f1a\np0\n.')
        try:
            x = mc.set((s * SERVER_MAX_KEY_LENGTH).encode('utf-8'), 1)
        except Client.MemcachedKeyLengthError:
            print("OK")
        else:
            print("FAIL")
            failures += 1

        print("Testing using a value larger than the memcached value limit...")
        print('NOTE: "MemCached: while expecting[...]" is normal...')
        x = mc.set('keyhere', 'a'*SERVER_MAX_VALUE_LENGTH)
        if mc.get('keyhere') is None:
            print("OK", end=" ")
        else:
            print("FAIL", end=" ")
            failures += 1
        x = mc.set('keyhere', 'a'*SERVER_MAX_VALUE_LENGTH + 'aaa')
        if mc.get('keyhere') is None:
            print("OK")
        else:
            print("FAIL")
            failures += 1

        print("Testing set_multi() with no memcacheds running", end=" ")
        mc.disconnect_all()
        errors = mc.set_multi({'keyhere': 'a', 'keythere': 'b'})
        if errors != []:
            print("FAIL")
            failures += 1
        else:
            print("OK")

        print("Testing delete_multi() with no memcacheds running", end=" ")
        mc.disconnect_all()
        ret = mc.delete_multi({'keyhere': 'a', 'keythere': 'b'})
        if ret != 1:
            print("FAIL")
            failures += 1
        else:
            print("OK")

    if failures > 0:
        print('*** THERE WERE FAILED TESTS')
        sys.exit(1)
    sys.exit(0)


# vim: ts=4 sw=4 et :
