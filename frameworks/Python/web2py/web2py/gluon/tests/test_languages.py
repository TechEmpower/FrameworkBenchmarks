#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
    Unit tests for gluon.languages
"""

import sys
import os
import tempfile
import unittest
from fix_path import fix_sys_path

fix_sys_path(__file__)

#support skipif also in python 2.6
def _skipIf(cond, message=''):
    def _decorator(testcase):
        if cond:
            return lambda *a, **kw: None
        else:
            return testcase
    return _decorator

if hasattr(unittest, 'skipIf'):
    skipIf = unittest.skipIf
else:
    skipIf = _skipIf

import languages
MP_WORKING = 0
try:
    import multiprocessing
    MP_WORKING = 1
    #due to http://bugs.python.org/issue10845, testing multiprocessing in python is impossible
    if sys.platform.startswith('win'):
        MP_WORKING = 0
    #multiprocessing is also not available on GAE. Since tests randomly
    #fail, let's not make them on it too
    if 'datastore' in os.getenv('DB', ''):
        MP_WORKING = 0
except ImportError:
    pass


def read_write(args):
    (filename, iterations) = args
    for i in range(0, iterations):
        content = languages.read_dict(filename)
        if not len(content):
            return False
        languages.write_dict(filename, content)
    return True


class TestLanguagesParallel(unittest.TestCase):

    def setUp(self):
        self.filename = tempfile.mktemp()
        contents = dict()
        for i in range(1000):
            contents["key%d" % i] = "value%d" % i
        languages.write_dict(self.filename, contents)
        languages.read_dict(self.filename)

    def tearDown(self):
        try:
            os.remove(self.filename)
        except:
            pass

    @skipIf(MP_WORKING == 0, 'multiprocessing tests unavailable')
    def test_reads_and_writes(self):
        readwriters = 10
        pool = multiprocessing.Pool(processes=readwriters)
        results = pool.map(read_write, [[self.filename, 10]] * readwriters)
        for result in results:
            self.assertTrue(result)

    @skipIf(MP_WORKING == 1, 'multiprocessing tests available')
    def test_reads_and_writes_no_mp(self):
        results = []
        for i in range(10):
            results.append(read_write([self.filename, 10]))
        for result in results:
            self.assertTrue(result)


class TestTranslations(unittest.TestCase):

    def setUp(self):
        if os.path.isdir('gluon'):
            self.langpath = 'applications/welcome/languages'
        else:
            self.langpath = os.path.realpath(
                '../../applications/welcome/languages')
        self.http_accept_language = 'en'

    def tearDown(self):
        pass

    def test_plain(self):
        T = languages.translator(self.langpath, self.http_accept_language)
        self.assertEqual(str(T('Hello World')),
                         'Hello World')
        self.assertEqual(str(T('Hello World## comment')),
                         'Hello World')
        self.assertEqual(str(T('%s %%{shop}', 1)),
                         '1 shop')
        self.assertEqual(str(T('%s %%{shop}', 2)),
                         '2 shops')
        self.assertEqual(str(T('%s %%{shop[0]}', 1)),
                         '1 shop')
        self.assertEqual(str(T('%s %%{shop[0]}', 2)),
                         '2 shops')
        self.assertEqual(str(T('%s %%{quark[0]}', 1)),
                         '1 quark')
        self.assertEqual(str(T('%s %%{quark[0]}', 2)),
                         '2 quarks')
        self.assertEqual(str(T.M('**Hello World**')),
                         '<strong>Hello World</strong>')
        T.force('it')
        self.assertEqual(str(T('Hello World')),
                         'Salve Mondo')

if __name__ == '__main__':
    unittest.main()
