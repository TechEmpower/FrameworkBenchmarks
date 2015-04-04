#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Unit tests for gluon.template
"""

import unittest
from fix_path import fix_sys_path

fix_sys_path(__file__)

from template import render


class TestVirtualFields(unittest.TestCase):

    def testRun(self):
        self.assertEqual(render(content='{{for i in range(n):}}{{=i}}{{pass}}',
                                context=dict(n=3)), '012')
        self.assertEqual(render(content='{{if n>2:}}ok{{pass}}',
                                context=dict(n=3)), 'ok')
        self.assertEqual(
            render(content='{{try:}}{{n/0}}{{except:}}fail{{pass}}',
                   context=dict(n=3)), 'fail')
        self.assertEqual(render(content='{{="<&>"}}'), '&lt;&amp;&gt;')
        self.assertEqual(render(content='"abc"'), '"abc"')
        self.assertEqual(render(content='"a\'bc"'), '"a\'bc"')
        self.assertEqual(render(content='"a\"bc"'), '"a\"bc"')
        self.assertEqual(render(content=r'''"a\"bc"'''), r'"a\"bc"')
        self.assertEqual(render(content=r'''"""abc\""""'''), r'"""abc\""""')

    def testEqualWrite(self):
        "test generation of response.write from ="
        self.assertEqual(render(content='{{="abc"}}'), 'abc')
        # whitespace is stripped
        self.assertEqual(render(content='{{ ="abc"}}'), 'abc')
        self.assertEqual(render(content='{{ ="abc" }}'), 'abc')
        self.assertEqual(render(content='{{pass\n="abc" }}'), 'abc')
        # = recognized only at the beginning of a physical line
        self.assertEqual(render(
            content='{{xyz = "xyz"\n="abc"\n="def"\n=xyz }}'), 'abcdefxyz')
        # = in python blocks
        self.assertEqual(render(content='{{if True:\n="abc"\npass }}'), 'abc')
        self.assertEqual(
            render(content='{{if True:\n="abc"\npass\n="def" }}'), 'abcdef')
        self.assertEqual(
            render(content='{{if False:\n="abc"\npass\n="def" }}'), 'def')
        self.assertEqual(render(
            content='{{if True:\n="abc"\nelse:\n="def"\npass }}'), 'abc')
        self.assertEqual(render(
            content='{{if False:\n="abc"\nelse:\n="def"\npass }}'), 'def')
        # codeblock-leading = handles internal newlines, escaped or not
        self.assertEqual(render(content='{{=list((1,2,3))}}'), '[1, 2, 3]')
        self.assertEqual(render(content='{{=list((1,2,\\\n3))}}'), '[1, 2, 3]')
        self.assertEqual(render(content='{{=list((1,2,\n3))}}'), '[1, 2, 3]')
        # ...but that means no more = operators in the codeblock
        self.assertRaises(SyntaxError, render, content='{{="abc"\n="def" }}')
        # = embedded in codeblock won't handle newlines in its argument
        self.assertEqual(
            render(content='{{pass\n=list((1,2,\\\n3))}}'), '[1, 2, 3]')
        self.assertRaises(
            SyntaxError, render, content='{{pass\n=list((1,2,\n3))}}')


if __name__ == '__main__':
    unittest.main()
