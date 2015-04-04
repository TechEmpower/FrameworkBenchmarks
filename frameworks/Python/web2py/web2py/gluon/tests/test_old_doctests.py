#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Unit tests for old doctests in validators.py, utf8.py, html.py,
    markmin2html.py.
    Don't abuse doctests, web2py > 2.4.5 will accept only unittests
"""
import sys
import os
import doctest
import unittest
from fix_path import fix_sys_path

fix_sys_path(__file__)


def load_tests(loader, tests, ignore):

    tests.addTests(
        doctest.DocTestSuite('html')
        )
    tests.addTests(
        doctest.DocTestSuite('utf8')
        )

    tests.addTests(
        doctest.DocTestSuite('contrib.markmin.markmin2html',
        )
    )

    return tests

if __name__ == '__main__':
    unittest.main()
