#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Unit tests for contribs """

import unittest
import os
from fix_path import fix_sys_path

fix_sys_path(__file__)

from gluon.storage import Storage
import gluon.contrib.fpdf as fpdf
import gluon.contrib.pyfpdf as pyfpdf
from gluon.contrib.appconfig import AppConfig


def setUpModule():
    pass


def tearDownModule():
    if os.path.isfile('appconfig.json'):
        os.unlink('appconfig.json')


class TestContribs(unittest.TestCase):
    """ Tests the contrib package """

    def test_fpdf(self):
        """ Basic PDF test and sanity checks """

        self.assertEqual(
            fpdf.FPDF_VERSION, pyfpdf.FPDF_VERSION, 'version mistmatch')
        self.assertEqual(fpdf.FPDF, pyfpdf.FPDF, 'class mistmatch')

        pdf = fpdf.FPDF()
        pdf.add_page()
        pdf.compress = False
        pdf.set_font('Arial', '', 14)
        pdf.ln(10)
        pdf.write(5, 'hello world')
        pdf_out = pdf.output('', 'S')

        self.assertTrue(fpdf.FPDF_VERSION in pdf_out, 'version string')
        self.assertTrue('hello world' in pdf_out, 'sample message')

    def test_appconfig(self):
        """
        Test for the appconfig module
        """
        from gluon import current
        s = Storage({'application': 'admin',
                     'folder': 'applications/admin'})
        current.request = s
        simple_config = '{"config1" : "abc", "config2" : "bcd", "config3" : { "key1" : 1, "key2" : 2} }'
        with open('appconfig.json', 'w') as g:
            g.write(simple_config)
        myappconfig = AppConfig('appconfig.json')
        self.assertEqual(myappconfig['config1'], 'abc')
        self.assertEqual(myappconfig['config2'], 'bcd')
        self.assertEqual(myappconfig.take('config1'), 'abc')
        self.assertEqual(myappconfig.take('config3.key1', cast=str), '1')
        # once parsed, can't be casted to other types
        self.assertEqual(myappconfig.take('config3.key1', cast=int), '1')

        self.assertEqual(myappconfig.take('config3.key2'), 2)

        current.request = {}

if __name__ == '__main__':
    unittest.main()
