#!/bin/python
# -*- coding: utf-8 -*-

"""
    Unit tests for gluon.html
"""

import unittest
from fix_path import fix_sys_path

fix_sys_path(__file__)

from html import *
from html import verifyURL
from storage import Storage


class TestBareHelpers(unittest.TestCase):

    def testBR(self):
        self.assertEqual(BR(_a='1', _b='2').xml(), '<br a="1" b="2" />')

    def testEMBED(self):
        self.assertEqual(EMBED(_a='1', _b='2').xml(),
                         '<embed a="1" b="2" />')

    def testHR(self):
        self.assertEqual(HR(_a='1', _b='2').xml(), '<hr a="1" b="2" />')

    def testIMG(self):
        self.assertEqual(IMG(_a='1', _b='2').xml(),
                         '<img a="1" b="2" />')

    def testINPUT(self):
        self.assertEqual(INPUT(_a='1', _b='2').xml(),
                         '<input a="1" b="2" type="text" />')

    def testLINK(self):
        self.assertEqual(LINK(_a='1', _b='2').xml(),
                         '<link a="1" b="2" />')

    def testMETA(self):
        self.assertEqual(META(_a='1', _b='2').xml(),
                         '<meta a="1" b="2" />')

    def testA(self):
        self.assertEqual(
            A('<>', _a='1', _b='2').xml(),
            '<a a="1" b="2">&lt;&gt;</a>'
            )
        self.assertEqual(
            A('a', cid='b').xml(),
            '<a data-w2p_disable_with="default" data-w2p_method="GET" data-w2p_target="b">a</a>'
            )
        self.assertEqual(
            A('a', callback='b', _id='c').xml(),
            '<a data-w2p_disable_with="default" data-w2p_method="POST" href="b" id="c">a</a>'
            )
        self.assertEqual(
            A('a', delete='tr').xml(),
            '<a data-w2p_disable_with="default" data-w2p_remove="tr">a</a>'
            )
        self.assertEqual(
            A('a', _id='b', target='<self>').xml(),
            '<a data-w2p_disable_with="default" data-w2p_target="b" id="b">a</a>'
            )
        self.assertEqual(
            A('a', component='b').xml(),
            '<a data-w2p_disable_with="default" data-w2p_method="GET" href="b">a</a>'
            )
        self.assertEqual(
            A('a', _id='b', callback='c', noconfirm=True).xml(),
            '<a data-w2p_disable_with="default" data-w2p_method="POST" href="c" id="b">a</a>'
            )
        self.assertEqual(
            A('a', cid='b').xml(),
            '<a data-w2p_disable_with="default" data-w2p_method="GET" data-w2p_target="b">a</a>'
            )
        self.assertEqual(
            A('a', cid='b', _disable_with='processing...').xml(),
            '<a data-w2p_disable_with="processing..." data-w2p_method="GET" data-w2p_target="b">a</a>'
            )
        self.assertEqual(
            A('a', callback='b', delete='tr', noconfirm=True, _id='c').xml(),
            '<a data-w2p_disable_with="default" data-w2p_method="POST" data-w2p_remove="tr" href="b" id="c">a</a>'
            )
        self.assertEqual(
            A('a', callback='b', delete='tr', confirm='Are you sure?', _id='c').xml(),
            '<a data-w2p_confirm="Are you sure?" data-w2p_disable_with="default" data-w2p_method="POST" data-w2p_remove="tr" href="b" id="c">a</a>'
            )

    def testB(self):
        self.assertEqual(B('<>', _a='1', _b='2').xml(),
                         '<b a="1" b="2">&lt;&gt;</b>')

    def testBODY(self):
        self.assertEqual(BODY('<>', _a='1', _b='2').xml(),
                         '<body a="1" b="2">&lt;&gt;</body>')

    def testCENTER(self):
        self.assertEqual(CENTER('<>', _a='1', _b='2').xml(),
                         '<center a="1" b="2">&lt;&gt;</center>')

    def testDIV(self):
        self.assertEqual(DIV('<>', _a='1', _b='2').xml(),
                         '<div a="1" b="2">&lt;&gt;</div>')
        # attributes can be updated like in a dict
        div = DIV('<>', _a='1')
        div['_b'] = '2'
        self.assertEqual(div.xml(),
                         '<div a="1" b="2">&lt;&gt;</div>')
        # also with a mapping
        div.update(_b=2, _c=3)
        self.assertEqual(div.xml(),
                         '<div a="1" b="2" c="3">&lt;&gt;</div>')
        # length of the DIV is the number of components
        self.assertEqual(len(DIV('a', 'bc')), 2)
        # also if empty, DIV is True in a boolean evaluation
        self.assertTrue(True if DIV() else False)
        # parent and siblings
        a = DIV(SPAN('a'), DIV('b'))
        s = a.element('span')
        d = s.parent
        d['_class'] = 'abc'
        self.assertEqual(a.xml(), '<div class="abc"><span>a</span><div>b</div></div>')
        self.assertEqual([el.xml() for el in s.siblings()], ['<div>b</div>'])
        self.assertEqual(s.sibling().xml(), '<div>b</div>')
        self.assertEqual(s.siblings('a'), [])

    def testEM(self):
        self.assertEqual(EM('<>', _a='1', _b='2').xml(),
                         '<em a="1" b="2">&lt;&gt;</em>')

    def testFIELDSET(self):
        self.assertEqual(FIELDSET('<>', _a='1', _b='2').xml(),
                         '<fieldset a="1" b="2">&lt;&gt;</fieldset>')

    def testFORM(self):
        self.assertEqual(FORM('<>', _a='1', _b='2').xml(),
                         '<form a="1" action="#" b="2" enctype="multipart/form-data" method="post">&lt;&gt;</form>')

    def testH1(self):
        self.assertEqual(H1('<>', _a='1', _b='2').xml(),
                         '<h1 a="1" b="2">&lt;&gt;</h1>')

    def testH2(self):
        self.assertEqual(H2('<>', _a='1', _b='2').xml(),
                         '<h2 a="1" b="2">&lt;&gt;</h2>')

    def testH3(self):
        self.assertEqual(H3('<>', _a='1', _b='2').xml(),
                         '<h3 a="1" b="2">&lt;&gt;</h3>')

    def testH4(self):
        self.assertEqual(H4('<>', _a='1', _b='2').xml(),
                         '<h4 a="1" b="2">&lt;&gt;</h4>')

    def testH5(self):
        self.assertEqual(H5('<>', _a='1', _b='2').xml(),
                         '<h5 a="1" b="2">&lt;&gt;</h5>')

    def testH6(self):
        self.assertEqual(H6('<>', _a='1', _b='2').xml(),
                         '<h6 a="1" b="2">&lt;&gt;</h6>')

    def testHEAD(self):
        self.assertEqual(HEAD('<>', _a='1', _b='2').xml(),
                         '<head a="1" b="2">&lt;&gt;</head>')

    def testHTML(self):
        self.assertEqual(HTML('<>', _a='1', _b='2').xml(),
                         '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">\n<html a="1" b="2" lang="en">&lt;&gt;</html>')

    def testIFRAME(self):
        self.assertEqual(IFRAME('<>', _a='1', _b='2').xml(),
                         '<iframe a="1" b="2">&lt;&gt;</iframe>')

    def testLABEL(self):
        self.assertEqual(LABEL('<>', _a='1', _b='2').xml(),
                         '<label a="1" b="2">&lt;&gt;</label>')

    def testLI(self):
        self.assertEqual(LI('<>', _a='1', _b='2').xml(),
                         '<li a="1" b="2">&lt;&gt;</li>')

    def testOBJECT(self):
        self.assertEqual(OBJECT('<>', _a='1', _b='2').xml(),
                         '<object a="1" b="2">&lt;&gt;</object>')

    def testOL(self):
        self.assertEqual(OL('<>', _a='1', _b='2').xml(),
                         '<ol a="1" b="2"><li>&lt;&gt;</li></ol>')

    def testOPTION(self):
        self.assertEqual(OPTION('<>', _a='1', _b='2').xml(),
                         '<option a="1" b="2" value="&lt;&gt;">&lt;&gt;' +
                         '</option>')

    def testP(self):
        self.assertEqual(P('<>', _a='1', _b='2').xml(),
                         '<p a="1" b="2">&lt;&gt;</p>')
        # test cr2br
        self.assertEqual(P('a\nb').xml(), '<p>a\nb</p>')
        self.assertEqual(P('a\nb', cr2br=True).xml(), '<p>a<br />b</p>')

    def testPRE(self):
        self.assertEqual(PRE('<>', _a='1', _b='2').xml(),
                         '<pre a="1" b="2">&lt;&gt;</pre>')

    def testSCRIPT(self):
        self.assertEqual(SCRIPT('<>', _a='1', _b='2').xml(),
                         '''<script a="1" b="2"><!--
<>
//--></script>''')
        self.assertEqual(SCRIPT('<>').xml(),
                         '''<script><!--
<>
//--></script>''')
        self.assertEqual(SCRIPT().xml(), '<script></script>')

    def testSELECT(self):
        self.assertEqual(SELECT('<>', _a='1', _b='2').xml(),
                         '<select a="1" b="2">' +
                         '<option value="&lt;&gt;">&lt;&gt;</option></select>')

    def testSPAN(self):
        self.assertEqual(SPAN('<>', _a='1', _b='2').xml(),
                         '<span a="1" b="2">&lt;&gt;</span>')

    def testSTYLE(self):
        self.assertEqual(STYLE('<>', _a='1', _b='2').xml(),
                         '<style a="1" b="2"><!--/*--><![CDATA[/*><!--*/\n<>\n/*]]>*/--></style>')

    def testTABLE(self):
        self.assertEqual(TABLE('<>', _a='1', _b='2').xml(),
                         '<table a="1" b="2"><tr><td>&lt;&gt;</td></tr>' +
                         '</table>')

    def testTBODY(self):
        self.assertEqual(TBODY('<>', _a='1', _b='2').xml(),
                         '<tbody a="1" b="2"><tr><td>&lt;&gt;</td></tr></tbody>')

    def testTD(self):
        self.assertEqual(TD('<>', _a='1', _b='2').xml(),
                         '<td a="1" b="2">&lt;&gt;</td>')

    def testTEXTAREA(self):
        self.assertEqual(TEXTAREA('<>', _a='1', _b='2').xml(),
                         '<textarea a="1" b="2" cols="40" rows="10">&lt;&gt;' +
                         '</textarea>')
        # override _rows and _cols
        self.assertEqual(TEXTAREA('<>', _a='1', _b='2', _rows=5, _cols=20).xml(),
                         '<textarea a="1" b="2" cols="20" rows="5">&lt;&gt;' +
                         '</textarea>')

    def testTFOOT(self):
        self.assertEqual(TFOOT('<>', _a='1', _b='2').xml(),
                         '<tfoot a="1" b="2"><tr><td>&lt;&gt;</td></tr></tfoot>')

    def testTH(self):
        self.assertEqual(TH('<>', _a='1', _b='2').xml(),
                         '<th a="1" b="2">&lt;&gt;</th>')

    def testTHEAD(self):
        self.assertEqual(THEAD('<>', _a='1', _b='2').xml(),
                         '<thead a="1" b="2"><tr><th>&lt;&gt;</th></tr></thead>')
        #self.assertEqual(THEAD(TRHEAD('<>'), _a='1', _b='2').xml(),
        #                 '<thead a="1" b="2"><tr><th>&lt;&gt;</th></tr></thead>')
        self.assertEqual(THEAD(TR('<>'), _a='1', _b='2').xml(),
                         '<thead a="1" b="2"><tr><td>&lt;&gt;</td></tr></thead>')

    def testTITLE(self):
        self.assertEqual(TITLE('<>', _a='1', _b='2').xml(),
                         '<title a="1" b="2">&lt;&gt;</title>')

    def testTR(self):
        self.assertEqual(TR('<>', _a='1', _b='2').xml(),
                         '<tr a="1" b="2"><td>&lt;&gt;</td></tr>')

    def testTT(self):
        self.assertEqual(TT('<>', _a='1', _b='2').xml(),
                         '<tt a="1" b="2">&lt;&gt;</tt>')

    def testUL(self):
        self.assertEqual(UL('<>', _a='1', _b='2').xml(),
                         '<ul a="1" b="2"><li>&lt;&gt;</li></ul>')

    def testXML(self):
        # sanitization process
        self.assertEqual(XML('<h1>Hello<a data-hello="world">World</a></h1>').xml(),
            '<h1>Hello<a data-hello="world">World</a></h1>')
        # with sanitize, data-attributes are not permitted
        self.assertEqual(XML('<h1>Hello<a data-hello="world">World</a></h1>', sanitize=True).xml(),
            '<h1>HelloWorld</h1>')
        # stringify by default
        self.assertEqual(XML(1.3), '1.3')
        self.assertEqual(XML(u'<div>è</div>').xml(), '<div>\xc3\xa8</div>')
        # you can calc len on the class, that equals the xml() and the str()
        self.assertEqual(len(XML('1.3')), len('1.3'))
        self.assertEqual(len(XML('1.3').xml()), len('1.3'))
        self.assertEqual(len(str(XML('1.3'))), len('1.3'))
        # you can concatenate them to strings (check for __add__ and __radd__ methods)
        self.assertEqual(XML('a') + 'b', 'ab')
        self.assertEqual(XML('a') + XML('b'), 'ab')
        self.assertEqual('a' + XML('b'), 'ab')
        # you can compare them
        self.assertEqual(XML('a') == XML('a'), True)
        # beware that the comparison is made on the XML repr
        self.assertEqual(XML('<h1>Hello<a data-hello="world">World</a></h1>', sanitize=True),
            XML('<h1>HelloWorld</h1>'))
        #bug check for the sanitizer for closing no-close tags
        self.assertEqual(XML('<p>Test</p><br/><p>Test</p><br/>', sanitize=True), 
            XML('<p>Test</p><br /><p>Test</p><br />'))

    def testTAG(self):
        self.assertEqual(TAG.first(TAG.second('test'), _key=3).xml(),
            '<first key="3"><second>test</second></first>')
        # ending in underscore "triggers" <input /> style
        self.assertEqual(TAG.first_(TAG.second('test'), _key=3).xml(),
            '<first key="3" />')

    def testStaticURL(self):
        # test response.static_version coupled with response.static_version_urls
        self.assertEqual(URL('a', 'c', 'f'), '/a/c/f')
        self.assertEqual(URL('a', 'static', 'design.css'), '/a/static/design.css')
        response = Storage()
        response.static_version = '1.2.3'
        from globals import current
        current.response = response
        self.assertEqual(URL('a', 'static', 'design.css'), '/a/static/design.css')
        response.static_version_urls = True
        self.assertEqual(URL('a', 'static', 'design.css'), '/a/static/_1.2.3/design.css')

    def testURL(self):
        self.assertEqual(URL('a', 'c', 'f', args='1'), '/a/c/f/1')
        self.assertEqual(URL('a', 'c', 'f', args=('1', '2')), '/a/c/f/1/2')
        self.assertEqual(URL('a', 'c', 'f', args=['1', '2']), '/a/c/f/1/2')
        self.assertEqual(URL('a', 'c', '/f'), '/a/c/f')
        self.assertEqual(URL('a', 'c', 'f.json'), '/a/c/f.json')
        self.assertRaises(SyntaxError, URL, *['a'])
        request = Storage()
        request.application = 'a'
        request.controller = 'c'
        request.function = 'f'
        request.env = {}
        from globals import current
        current.request = request
        must_return = '/a/c/f'
        self.assertEqual(URL(), must_return)
        self.assertEqual(URL('f'), must_return)
        self.assertEqual(URL('c', 'f'), must_return)
        self.assertEqual(URL('a', 'c', 'f'), must_return)
        self.assertEqual(URL('a', 'c', 'f', extension='json'), '/a/c/f.json')
        def weird():
            pass
        self.assertEqual(URL('a', 'c', weird), '/a/c/weird')
        self.assertRaises(SyntaxError, URL, *['a', 'c', 1])
        # test signature
        rtn = URL(
            a='a', c='c', f='f', args=['x', 'y', 'z'],
            vars={'p': (1, 3), 'q': 2}, anchor='1', hmac_key='key'
            )
        self.assertEqual(rtn, '/a/c/f/x/y/z?p=1&p=3&q=2&_signature=a32530f0d0caa80964bb92aad2bedf8a4486a31f#1')
        # test _signature exclusion
        rtn = URL(
            a='a', c='c', f='f', args=['x', 'y', 'z'],
            vars={'p': (1, 3), 'q': 2, '_signature': 'abc'},
            anchor='1', hmac_key='key'
            )
        self.assertEqual(rtn, '/a/c/f/x/y/z?p=1&p=3&q=2&_signature=a32530f0d0caa80964bb92aad2bedf8a4486a31f#1')
        # emulate user_signature
        current.session = Storage(auth=Storage(hmac_key='key'))
        self.assertEqual(URL(user_signature=True), '/a/c/f?_signature=c4aed53c08cff08f369dbf8b5ba51889430cf2c2')
        # hash_vars combination
        rtn = URL('a','c','f', args=['x', 'y', 'z'], vars={'p' : (1,3), 'q' : 2}, hmac_key='key')
        self.assertEqual(rtn, '/a/c/f/x/y/z?p=1&p=3&q=2&_signature=a32530f0d0caa80964bb92aad2bedf8a4486a31f')
        rtn = URL('a','c','f', args=['x', 'y', 'z'], vars={'p' : (1,3), 'q' : 2}, hmac_key='key', hash_vars=True)
        self.assertEqual(rtn, '/a/c/f/x/y/z?p=1&p=3&q=2&_signature=a32530f0d0caa80964bb92aad2bedf8a4486a31f')
        rtn = URL('a','c','f', args=['x', 'y', 'z'], vars={'p' : (1,3), 'q' : 2}, hmac_key='key', hash_vars=False)
        self.assertEqual(rtn, '/a/c/f/x/y/z?p=1&p=3&q=2&_signature=0b5a0702039992aad23c82794b8496e5dcd59a5b')
        rtn = URL('a','c','f', args=['x', 'y', 'z'], vars={'p' : (1,3), 'q' : 2}, hmac_key='key', hash_vars=['p'])
        self.assertEqual(rtn, '/a/c/f/x/y/z?p=1&p=3&q=2&_signature=5d01b982fd72b39674b012e0288071034e156d7a')
        rtn = URL('a','c','f', args=['x', 'y', 'z'], vars={'p' : (1,3), 'q' : 2}, hmac_key='key', hash_vars='p')
        self.assertEqual(rtn, '/a/c/f/x/y/z?p=1&p=3&q=2&_signature=5d01b982fd72b39674b012e0288071034e156d7a')
        # test CRLF detection
        self.assertRaises(SyntaxError, URL, *['a\n', 'c', 'f'])
        self.assertRaises(SyntaxError, URL, *['a\r', 'c', 'f'])

    def testverifyURL(self):
        r = Storage()
        r.application = 'a'
        r.controller = 'c'
        r.function = 'f'
        r.extension = 'html'
        r.env = {}
        r.get_vars = Storage()
        # missing signature as request.get_vars returns False
        rtn = verifyURL(r, 'key')
        self.assertEqual(rtn, False)
        # reverse tests from previous testcase with hash_vars combinations
        r.args = ['x', 'y', 'z']
        r.get_vars = Storage(p=(1, 3), q=2)
        # add signature
        r.get_vars['_signature'] = 'a32530f0d0caa80964bb92aad2bedf8a4486a31f'
        rtn = verifyURL(r, 'key')
        self.assertEqual(rtn, True)
        r.get_vars['_signature'] = 'a32530f0d0caa80964bb92aad2bedf8a4486a31f'
        rtn = verifyURL(r, 'key', hash_vars=True)
        self.assertEqual(rtn, True)
        r.get_vars['_signature'] = '0b5a0702039992aad23c82794b8496e5dcd59a5b'
        rtn = verifyURL(r, 'key', hash_vars=False)
        self.assertEqual(rtn, True)
        r.get_vars['_signature'] = '5d01b982fd72b39674b012e0288071034e156d7a'
        rtn = verifyURL(r, 'key', hash_vars=['p'])
        self.assertEqual(rtn, True)
        r.get_vars['_signature'] = '5d01b982fd72b39674b012e0288071034e156d7a'
        rtn = verifyURL(r, 'key', hash_vars='p')
        self.assertEqual(rtn, True)
        # without session, user_signature returns always False
        rtn = verifyURL(r, user_signature=True)
        self.assertEqual(rtn, False)
        # same goes if you don't use an hmac_key
        rtn = verifyURL(r)
        self.assertEqual(rtn, False)
        # emulate user signature
        from globals import current
        current.session = Storage(auth=Storage(hmac_key='key'))
        r.get_vars['_signature'] = 'a32530f0d0caa80964bb92aad2bedf8a4486a31f'
        rtn = verifyURL(r, user_signature=True)
        self.assertEqual(rtn, True)


class TestData(unittest.TestCase):

    def testAdata(self):
        self.assertEqual(A('<>', data=dict(abc='<def?asd>', cde='standard'), _a='1', _b='2').xml(),
            '<a a="1" b="2" data-abc="&lt;def?asd&gt;" data-cde="standard">&lt;&gt;</a>')


if __name__ == '__main__':
    unittest.main()
