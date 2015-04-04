#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Unit tests for http.py """

import unittest
from fix_path import fix_sys_path

fix_sys_path(__file__)


from http import HTTP, defined_status


class TestHTTP(unittest.TestCase):
    """ Tests http.HTTP """

    def test_status_message(self):
        """ Tests http status code message """

        h = HTTP

        def gen_status_str(code, message):
            return str(code) + ' ' + str(message)
        message = '1423 This is a custom message'
        code = 1423
        self.assertEqual(str(h(gen_status_str(code, message))),
                         gen_status_str(code, message))

        # test predefined codes
        for code in defined_status.keys():
            self.assertEqual(
                str(h(code)),
                gen_status_str(code, defined_status[code]))

        # test correct use of status_message
        for code in defined_status.keys():
            self.assertEqual(str(h(gen_status_str(code, message))),
                             gen_status_str(code, message))

        # test wrong call detection

if __name__ == '__main__':
    unittest.main()
