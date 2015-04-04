#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
    Unit tests for gluon.contenttype
"""

import unittest
from fix_path import fix_sys_path

fix_sys_path(__file__)

from contenttype import contenttype


class TestContentType(unittest.TestCase):

    def testTypeRecognition(self):
        rtn = contenttype('.png')
        self.assertEqual(rtn, 'image/png')
        rtn = contenttype('.gif')
        self.assertEqual(rtn, 'image/gif')
        rtn = contenttype('.tar.bz2')
        self.assertEqual(rtn, 'application/x-bzip-compressed-tar')
        # test overrides and additions
        mapping = {
            '.load': 'text/html; charset=utf-8',
            '.json': 'application/json',
            '.jsonp': 'application/jsonp',
            '.pickle': 'application/python-pickle',
            '.w2p': 'application/w2p',
            '.md': 'text/x-markdown; charset=utf-8'
        }
        for k, v in mapping.iteritems():
            self.assertEqual(contenttype(k), v)

        # test without dot extension
        rtn = contenttype('png')
        self.assertEqual(rtn, 'text/plain; charset=utf-8')


if __name__ == '__main__':
    unittest.main()
