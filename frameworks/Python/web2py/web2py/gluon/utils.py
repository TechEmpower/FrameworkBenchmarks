#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

This file specifically includes utilities for security.
--------------------------------------------------------
"""

import threading
import struct
import uuid
import random
import inspect
import time
import os
import re
import sys
import logging
import socket
import base64
import zlib

_struct_2_long_long = struct.Struct('=QQ')

python_version = sys.version_info[0]

if python_version == 2:
    import cPickle as pickle
else:
    import pickle

import hashlib
from hashlib import md5, sha1, sha224, sha256, sha384, sha512

try:
    from Crypto.Cipher import AES
except ImportError:
    import gluon.contrib.aes as AES

import hmac

if hasattr(hashlib, "pbkdf2_hmac"):
    def pbkdf2_hex(data, salt, iterations=1000, keylen=24, hashfunc=None):
        hashfunc = hashfunc or sha1
        return hashlib.pbkdf2_hmac(hashfunc().name,
                           data, salt, iterations,
                           keylen).encode("hex")
    HAVE_PBKDF2 = True
else:
    try:
        try:
            from gluon.contrib.pbkdf2_ctypes import pbkdf2_hex
        except (ImportError, AttributeError):
            from gluon.contrib.pbkdf2 import pbkdf2_hex
        HAVE_PBKDF2 = True
    except ImportError:
        try:
            from .pbkdf2 import pbkdf2_hex
            HAVE_PBKDF2 = True
        except (ImportError, ValueError):
            HAVE_PBKDF2 = False

logger = logging.getLogger("web2py")


def AES_new(key, IV=None):
    """ Returns an AES cipher object and random IV if None specified """
    if IV is None:
        IV = fast_urandom16()

    return AES.new(key, AES.MODE_CBC, IV), IV


def compare(a, b):
    """ Compares two strings and not vulnerable to timing attacks """
    if len(a) != len(b):
        return False
    result = 0
    for x, y in zip(a, b):
        result |= ord(x) ^ ord(y)
    return result == 0


def md5_hash(text):
    """ Generates a md5 hash with the given text """
    return md5(text).hexdigest()


def simple_hash(text, key='', salt='', digest_alg='md5'):
    """
    Generates hash with the given text using the specified
    digest hashing algorithm
    """
    if not digest_alg:
        raise RuntimeError("simple_hash with digest_alg=None")
    elif not isinstance(digest_alg, str):  # manual approach
        h = digest_alg(text + key + salt)
    elif digest_alg.startswith('pbkdf2'):  # latest and coolest!
        iterations, keylen, alg = digest_alg[7:-1].split(',')
        return pbkdf2_hex(text, salt, int(iterations),
                          int(keylen), get_digest(alg))
    elif key:  # use hmac
        digest_alg = get_digest(digest_alg)
        h = hmac.new(key + salt, text, digest_alg)
    else:  # compatible with third party systems
        h = get_digest(digest_alg)()
        h.update(text + salt)
    return h.hexdigest()


def get_digest(value):
    """
    Returns a hashlib digest algorithm from a string
    """
    if not isinstance(value, str):
        return value
    value = value.lower()
    if value == "md5":
        return md5
    elif value == "sha1":
        return sha1
    elif value == "sha224":
        return sha224
    elif value == "sha256":
        return sha256
    elif value == "sha384":
        return sha384
    elif value == "sha512":
        return sha512
    else:
        raise ValueError("Invalid digest algorithm: %s" % value)

DIGEST_ALG_BY_SIZE = {
    128 / 4: 'md5',
    160 / 4: 'sha1',
    224 / 4: 'sha224',
    256 / 4: 'sha256',
    384 / 4: 'sha384',
    512 / 4: 'sha512',
}

def get_callable_argspec(fn):
    if inspect.isfunction(fn) or inspect.ismethod(fn):
        inspectable = fn
    elif inspect.isclass(fn):
        inspectable = fn.__init__
    elif hasattr(fn, '__call__'):
        inspectable = fn.__call__
    else:
        inspectable = fn
    return inspect.getargspec(inspectable)

def pad(s, n=32, padchar=' '):
    return s + (32 - len(s) % 32) * padchar


def secure_dumps(data, encryption_key, hash_key=None, compression_level=None):
    if not hash_key:
        hash_key = sha1(encryption_key).hexdigest()
    dump = pickle.dumps(data, pickle.HIGHEST_PROTOCOL)
    if compression_level:
        dump = zlib.compress(dump, compression_level)
    key = pad(encryption_key[:32])
    cipher, IV = AES_new(key)
    encrypted_data = base64.urlsafe_b64encode(IV + cipher.encrypt(pad(dump)))
    signature = hmac.new(hash_key, encrypted_data).hexdigest()
    return signature + ':' + encrypted_data


def secure_loads(data, encryption_key, hash_key=None, compression_level=None):
    if not ':' in data:
        return None
    if not hash_key:
        hash_key = sha1(encryption_key).hexdigest()
    signature, encrypted_data = data.split(':', 1)
    actual_signature = hmac.new(hash_key, encrypted_data).hexdigest()
    if not compare(signature, actual_signature):
        return None
    key = pad(encryption_key[:32])
    encrypted_data = base64.urlsafe_b64decode(encrypted_data)
    IV, encrypted_data = encrypted_data[:16], encrypted_data[16:]
    cipher, _ = AES_new(key, IV=IV)
    try:
        data = cipher.decrypt(encrypted_data)
        data = data.rstrip(' ')
        if compression_level:
            data = zlib.decompress(data)
        return pickle.loads(data)
    except Exception, e:
        return None

### compute constant CTOKENS


def initialize_urandom():
    """
    This function and the web2py_uuid follow from the following discussion:
    `http://groups.google.com/group/web2py-developers/browse_thread/thread/7fd5789a7da3f09`

    At startup web2py compute a unique ID that identifies the machine by adding
    uuid.getnode() + int(time.time() * 1e3)

    This is a 48-bit number. It converts the number into 16 8-bit tokens.
    It uses this value to initialize the entropy source ('/dev/urandom') and to seed random.

    If os.random() is not supported, it falls back to using random and issues a warning.
    """
    node_id = uuid.getnode()
    microseconds = int(time.time() * 1e6)
    ctokens = [((node_id + microseconds) >> ((i % 6) * 8)) %
               256 for i in range(16)]
    random.seed(node_id + microseconds)
    try:
        os.urandom(1)
        have_urandom = True
        try:
            # try to add process-specific entropy
            frandom = open('/dev/urandom', 'wb')
            try:
                if python_version == 2:
                    frandom.write(''.join(chr(t) for t in ctokens)) # python 2
                else:
                    frandom.write(bytes([]).join(bytes([t]) for t in ctokens)) # python 3
            finally:
                frandom.close()
        except IOError:
            # works anyway
            pass
    except NotImplementedError:
        have_urandom = False
        logger.warning(
            """Cryptographically secure session management is not possible on your system because
your system does not provide a cryptographically secure entropy source.
This is not specific to web2py; consider deploying on a different operating system.""")
    if python_version == 2:
        packed = ''.join(chr(x) for x in ctokens) # python 2
    else:
        packed = bytes([]).join(bytes([x]) for x in ctokens) # python 3
    unpacked_ctokens = _struct_2_long_long.unpack(packed)
    return unpacked_ctokens, have_urandom
UNPACKED_CTOKENS, HAVE_URANDOM = initialize_urandom()


def fast_urandom16(urandom=[], locker=threading.RLock()):
    """
    This is 4x faster than calling os.urandom(16) and prevents
    the "too many files open" issue with concurrent access to os.urandom()
    """
    try:
        return urandom.pop()
    except IndexError:
        try:
            locker.acquire()
            ur = os.urandom(16 * 1024)
            urandom += [ur[i:i + 16] for i in xrange(16, 1024 * 16, 16)]
            return ur[0:16]
        finally:
            locker.release()


def web2py_uuid(ctokens=UNPACKED_CTOKENS):
    """
    This function follows from the following discussion:
    `http://groups.google.com/group/web2py-developers/browse_thread/thread/7fd5789a7da3f09`

    It works like uuid.uuid4 except that tries to use os.urandom() if possible
    and it XORs the output with the tokens uniquely associated with this machine.
    """
    rand_longs = (random.getrandbits(64), random.getrandbits(64))
    if HAVE_URANDOM:
        urand_longs = _struct_2_long_long.unpack(fast_urandom16())
        byte_s = _struct_2_long_long.pack(rand_longs[0] ^ urand_longs[0] ^ ctokens[0],
                                          rand_longs[1] ^ urand_longs[1] ^ ctokens[1])
    else:
        byte_s = _struct_2_long_long.pack(rand_longs[0] ^ ctokens[0],
                                          rand_longs[1] ^ ctokens[1])
    return str(uuid.UUID(bytes=byte_s, version=4))

REGEX_IPv4 = re.compile('(\d+)\.(\d+)\.(\d+)\.(\d+)')


def is_valid_ip_address(address):
    """
    Examples:
        Better than a thousand words::

            >>> is_valid_ip_address('127.0')
            False
            >>> is_valid_ip_address('127.0.0.1')
            True
            >>> is_valid_ip_address('2001:660::1')
            True
    """
    # deal with special cases
    if address.lower() in ('127.0.0.1', 'localhost', '::1', '::ffff:127.0.0.1'):
        return True
    elif address.lower() in ('unknown', ''):
        return False
    elif address.count('.') == 3:  # assume IPv4
        if address.startswith('::ffff:'):
            address = address[7:]
        if hasattr(socket, 'inet_aton'):  # try validate using the OS
            try:
                socket.inet_aton(address)
                return True
            except socket.error:  # invalid address
                return False
        else:  # try validate using Regex
            match = REGEX_IPv4.match(address)
            if match and all(0 <= int(match.group(i)) < 256 for i in (1, 2, 3, 4)):
                return True
            return False
    elif hasattr(socket, 'inet_pton'):  # assume IPv6, try using the OS
        try:
            socket.inet_pton(socket.AF_INET6, address)
            return True
        except socket.error:  # invalid address
            return False
    else:  # do not know what to do? assume it is a valid address
        return True


def is_loopback_ip_address(ip=None, addrinfo=None):
    """
    Determines whether the address appears to be a loopback address.
    This assumes that the IP is valid.
    """
    if addrinfo: # see socket.getaddrinfo() for layout of addrinfo tuple
        if addrinfo[0] == socket.AF_INET or addrinfo[0] == socket.AF_INET6:
            ip = addrinfo[4]
    if not isinstance(ip, basestring):
        return False
    # IPv4 or IPv6-embedded IPv4 or IPv4-compatible IPv6
    if ip.count('.') == 3:
        return ip.lower().startswith(('127', '::127', '0:0:0:0:0:0:127',
                                      '::ffff:127', '0:0:0:0:0:ffff:127'))
    return ip == '::1' or ip == '0:0:0:0:0:0:0:1'   # IPv6 loopback


def getipaddrinfo(host):
    """
    Filter out non-IP and bad IP addresses from getaddrinfo
    """
    try:
        return [addrinfo for addrinfo in socket.getaddrinfo(host, None)
                if (addrinfo[0] == socket.AF_INET or
                    addrinfo[0] == socket.AF_INET6)
                and isinstance(addrinfo[4][0], basestring)]
    except socket.error:
        return []
