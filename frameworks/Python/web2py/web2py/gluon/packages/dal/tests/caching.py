import time
from pydal import DAL, Field
from ._compat import unittest
from ._adapt import DEFAULT_URI, IS_IMAP, drop


class SimpleCache(object):
    storage = {}

    def clear(self):
        self.storage.clear()

    def __call__(self, key, f, time_expire=300):
        dt = time_expire
        now = time.time()

        item = self.storage.get(key, None)
        if item and f is None:
            del self.storage[key]

        if f is None:
            return None
        if item and (dt is None or item[0] > now - dt):
            return item[1]

        value = f()
        self.storage[key] = (now, value)
        return value


@unittest.skipIf(IS_IMAP, "TODO: IMAP test")
class TestCache(unittest.TestCase):
    def testRun(self):
        cache = SimpleCache()
        db = DAL(DEFAULT_URI, check_reserved=['all'])
        db.define_table('tt', Field('aa'))
        db.tt.insert(aa='1')
        r0 = db().select(db.tt.ALL)
        r1 = db().select(db.tt.ALL, cache=(cache, 1000))
        self.assertEqual(len(r0), len(r1))
        r2 = db().select(db.tt.ALL, cache=(cache, 1000))
        self.assertEqual(len(r0), len(r2))
        r3 = db().select(db.tt.ALL, cache=(cache, 1000), cacheable=True)
        self.assertEqual(len(r0), len(r3))
        r4 = db().select(db.tt.ALL, cache=(cache, 1000), cacheable=True)
        self.assertEqual(len(r0), len(r4))
        drop(db.tt)
