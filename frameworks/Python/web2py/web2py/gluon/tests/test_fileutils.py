#!/usr/bin/env python
# -*- coding: utf-8 -*-

import unittest
import datetime
from fix_path import fix_sys_path

fix_sys_path(__file__)

from fileutils import parse_version


class TestFileUtils(unittest.TestCase):

    def testParseVersion(self):
        rtn = parse_version('Version 1.99.0-rc.1+timestamp.2011.09.19.08.23.26')
        self.assertEqual(rtn, (1, 99, 0, 'rc.1', datetime.datetime(2011, 9, 19, 8, 23, 26)))
        rtn = parse_version('Version 2.9.11-stable+timestamp.2014.09.15.18.31.17')
        self.assertEqual(rtn, (2, 9, 11, 'stable', datetime.datetime(2014, 9, 15, 18, 31, 17)))
        rtn = parse_version('Version 1.99.0 (2011-09-19 08:23:26)')
        self.assertEqual(rtn, (1, 99, 0, 'dev', datetime.datetime(2011, 9, 19, 8, 23, 26)))


if __name__ == '__main__':
    unittest.main()
