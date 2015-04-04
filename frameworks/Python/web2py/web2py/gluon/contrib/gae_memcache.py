#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Developed by Robin Bhattacharyya (memecache for GAE)
Released under the web2py license (LGPL)

from gluon.contrib.gae_memcache import MemcacheClient
cache.ram=cache.disk=MemcacheClient(request)
"""

import time
from google.appengine.api.memcache import Client


class MemcacheClient(object):

    client = Client()

    def __init__(self, request, default_time_expire = 300):
        self.request = request
        self.default_time_expire = default_time_expire

    def initialize(self):
        pass

    def __call__(
        self,
        key,
        f,
        time_expire=None,
    ):
        if time_expire is None:
            time_expire = self.default_time_expire

        key = '%s/%s' % (self.request.application, key)
        value = None
        obj = self.client.get(key) if time_expire != 0 else None
        if obj:
            value = obj[1]
        elif f is not None:
            value = f()
            self.client.set(key, (time.time(), value), time=time_expire)
        return value

    def increment(self, key, value=1):
        key = '%s/%s' % (self.request.application, key)
        obj = self.client.get(key)
        if obj:
            value = obj[1] + value
        self.client.set(key, (time.time(), value))
        return value

    def incr(self, key, value=1):
        return self.increment(key, value)

    def clear(self, key=None):
        if key:
            key = '%s/%s' % (self.request.application, key)
            self.client.delete(key)
        else:
            self.client.flush_all()

    def delete(self, *a, **b):
        return self.client.delete(*a, **b)

    def get(self, *a, **b):
        return self.client.get(*a, **b)

    def set(self, *a, **b):
        return self.client.set(*a, **b)

    def flush_all(self, *a, **b):
        return self.client.delete(*a, **b)
