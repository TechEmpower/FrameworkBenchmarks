#!/bin/python
# -*- coding: utf-8 -*-

"""
    Unit tests for gluon.serializers
"""

import unittest
from fix_path import fix_sys_path
import datetime
import decimal

fix_sys_path(__file__)

from serializers import *
from storage import Storage
# careful with the import path 'cause of isinstance() checks
from gluon.languages import translator
from gluon.html import SPAN


class TestSerializers(unittest.TestCase):

    def testJSON(self):
        # the main and documented "way" is to use the json() function
        # it has a few corner-cases that make json() be somewhat
        # different from the standard buyt being compliant
        # it's just a matter of conventions

        # incompatible spacing, newer simplejson already account
        # for this but it's still better to remember
        weird = {'JSON': u"ro" + u'\u2028' + u'ck' + u'\u2029' + u's!'}
        rtn = json(weird)
        self.assertEqual(rtn, u'{"JSON": "ro\\u2028ck\\u2029s!"}')
        # date, datetime, time strictly as strings in isoformat, minus the T
        objs = [
            datetime.datetime(2014, 1, 1, 12, 15, 35),
            datetime.date(2014, 1, 1),
            datetime.time(12, 15, 35)
            ]
        iso_objs = [obj.isoformat()[:19].replace('T', ' ') for obj in objs]
        json_objs = [json(obj) for obj in objs]
        json_web2pyfied = [json(obj) for obj in iso_objs]
        self.assertEqual(json_objs, json_web2pyfied)
        # int or long int()ified
        self.assertEqual(json(1), json(1L))
        # decimal stringified
        obj = {'a': decimal.Decimal('4.312312312312')}
        self.assertEqual(json(obj), u'{"a": "4.312312312312"}')
        # lazyT translated
        T = translator('', 'en')
        lazy_translation = T('abc')
        self.assertEqual(json(lazy_translation), u'"abc"')
        # html helpers are xml()ed before too
        self.assertEqual(json(SPAN('abc')), u'"<span>abc</span>"')
        # unicode keys make a difference with loads_json
        base = {u'è': 1, 'b': 2}
        base_enc = json(base)
        base_load = loads_json(base_enc)
        self.assertTrue(base == base_load)
        # if unicode_keys is false, the standard behaviour is assumed
        base_load = loads_json(base_enc, unicode_keys=False)
        self.assertFalse(base == base_load)


if __name__ == '__main__':
    unittest.main()
