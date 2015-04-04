#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Unit tests for IS_URL()
"""

import unittest
from fix_path import fix_sys_path

fix_sys_path(__file__)


from validators import IS_URL, IS_HTTP_URL, IS_GENERIC_URL
from validators import unicode_to_ascii_authority


class TestIsUrl(unittest.TestCase):

    def testModeHttp(self):

        # defaults to mode='http'

        x = IS_URL()
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         None))
        self.assertEqual(x('google.ca'), ('http://google.ca', None))
        self.assertEqual(x('google.ca:80'), ('http://google.ca:80',
                         None))
        self.assertEqual(x('unreal.blargg'), ('unreal.blargg',
                         'Enter a valid URL'))
        self.assertEqual(x('google..ca'), ('google..ca', 'Enter a valid URL'))
        self.assertEqual(
            x('google.ca..'), ('google.ca..', 'Enter a valid URL'))

        # explicit use of 'http' mode

        x = IS_URL(mode='http')
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         None))
        self.assertEqual(x('google.ca'), ('http://google.ca', None))
        self.assertEqual(x('google.ca:80'), ('http://google.ca:80',
                         None))
        self.assertEqual(x('unreal.blargg'), ('unreal.blargg',
                         'Enter a valid URL'))

        # prepends 'https' instead of 'http'

        x = IS_URL(mode='http', prepend_scheme='https')
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         None))
        self.assertEqual(x('google.ca'), ('https://google.ca', None))
        self.assertEqual(x('google.ca:80'), ('https://google.ca:80',
                         None))
        self.assertEqual(x('unreal.blargg'), ('unreal.blargg',
                         'Enter a valid URL'))

        # prepending disabled

        x = IS_URL(prepend_scheme=None)
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         None))
        self.assertEqual(x('google.ca'), ('google.ca', None))
        self.assertEqual(x('google.ca:80'), ('google.ca:80', None))
        self.assertEqual(x('unreal.blargg'), ('unreal.blargg',
                         'Enter a valid URL'))

        # custom allowed_schemes

        x = IS_URL(mode='http', allowed_schemes=[None, 'http'])
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         None))
        self.assertEqual(x('https://google.ca'), ('https://google.ca',
                         'Enter a valid URL'))
        self.assertEqual(x('google.ca'), ('http://google.ca', None))
        self.assertEqual(x('google.ca:80'), ('http://google.ca:80',
                         None))
        self.assertEqual(x('unreal.blargg'), ('unreal.blargg',
                         'Enter a valid URL'))

        # custom allowed_schemes, excluding None

        x = IS_URL(allowed_schemes=['http'])
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         None))
        self.assertEqual(x('https://google.ca'), ('https://google.ca',
                         'Enter a valid URL'))
        self.assertEqual(x('google.ca'), ('google.ca', 'Enter a valid URL'))
        self.assertEqual(x('google.ca:80'), ('google.ca:80',
                         'Enter a valid URL'))
        self.assertEqual(x('unreal.blargg'), ('unreal.blargg',
                         'Enter a valid URL'))

        # custom allowed_schemes and prepend_scheme

        x = IS_URL(allowed_schemes=[None, 'https'],
                   prepend_scheme='https')
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         'Enter a valid URL'))
        self.assertEqual(x('https://google.ca'), ('https://google.ca',
                         None))
        self.assertEqual(x('google.ca'), ('https://google.ca', None))
        self.assertEqual(x('google.ca:80'), ('https://google.ca:80',
                         None))
        self.assertEqual(x('unreal.blargg'), ('unreal.blargg',
                         'Enter a valid URL'))

        # Now any URL requiring prepending will fail, but prepending is still
        # enabled!

        x = IS_URL(allowed_schemes=['http'])
        self.assertEqual(x('google.ca'), ('google.ca', 'Enter a valid URL'))

    def testModeGeneric(self):

        # 'generic' mode

        x = IS_URL(mode='generic')
        self.assertEqual(x('http://google.ca'), ('http://google.ca', None))
        self.assertEqual(x('google.ca'), ('google.ca', None))
        self.assertEqual(x('google.ca:80'), ('http://google.ca:80', None))
        self.assertEqual(x('blargg://unreal'), ('blargg://unreal',
                         'Enter a valid URL'))

        # 'generic' mode with custom allowed_schemes that still includes
        # 'http' (the default for prepend_scheme)

        x = IS_URL(mode='generic', allowed_schemes=['http', 'blargg'])
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         None))
        self.assertEqual(x('ftp://google.ca'), ('ftp://google.ca',
                         'Enter a valid URL'))
        self.assertEqual(x('google.ca'), ('google.ca', 'Enter a valid URL'))
        self.assertEqual(x('google.ca:80'), ('google.ca:80',
                         'Enter a valid URL'))
        self.assertEqual(x('blargg://unreal'), ('blargg://unreal',
                         None))

        # 'generic' mode with overriden prepend_scheme

        x = IS_URL(mode='generic', prepend_scheme='ftp')
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         None))
        self.assertEqual(x('ftp://google.ca'), ('ftp://google.ca',
                         None))
        self.assertEqual(x('google.ca'), ('google.ca', None))
        self.assertEqual(x('google.ca:80'), ('ftp://google.ca:80',
                         None))
        self.assertEqual(x('blargg://unreal'), ('blargg://unreal',
                         'Enter a valid URL'))

        # 'generic' mode with overriden allowed_schemes and prepend_scheme

        x = IS_URL(mode='generic', allowed_schemes=[None, 'ftp', 'ftps'
                                                    ], prepend_scheme='ftp')
        self.assertEqual(x('http://google.ca'), ('http://google.ca',
                         'Enter a valid URL'))
        self.assertEqual(x('google.ca'), ('google.ca', None))
        self.assertEqual(x('ftp://google.ca'), ('ftp://google.ca',
                         None))
        self.assertEqual(x('google.ca:80'), ('ftp://google.ca:80',
                         None))
        self.assertEqual(x('blargg://unreal'), ('blargg://unreal',
                         'Enter a valid URL'))

        # Now any URL requiring prepending will fail, but prepending is still
        # enabled!

        x = IS_URL(mode='generic', allowed_schemes=['http'])
        self.assertEqual(x('google.ca'), ('google.ca', 'Enter a valid URL'))

    def testExceptionalUse(self):

        # mode must be in set ['http', 'generic']

        try:
            x = IS_URL(mode='ftp')
            x('http://www.google.ca')
        except Exception, e:
            if str(e) != "invalid mode 'ftp' in IS_URL":
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail("Accepted invalid mode: 'ftp'")

        # allowed_schemes in 'http' mode must be in set [None, 'http', 'https']

        try:
            x = IS_URL(allowed_schemes=[None, 'ftp', 'ftps'],
                       prepend_scheme='ftp')
            x('http://www.benn.ca')  # we can only reasonably know about the
                                     # error at calling time
        except Exception, e:
            if str(e)\
                    != "allowed_scheme value 'ftp' is not in [None, 'http', 'https']":
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail(
                "Accepted invalid allowed_schemes: [None, 'ftp', 'ftps']")

        # prepend_scheme's value must be in allowed_schemes (default for 'http'
        # mode is [None, 'http', 'https'])

        try:
            x = IS_URL(prepend_scheme='ftp')
            x('http://www.benn.ca')  # we can only reasonably know about the
                                     # error at calling time
        except Exception, e:
            if str(e)\
                    != "prepend_scheme='ftp' is not in allowed_schemes=[None, 'http', 'https']":
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail("Accepted invalid prepend_scheme: 'ftp'")

        # custom allowed_schemes that excludes 'http', so prepend_scheme must be
        # specified!

        try:
            x = IS_URL(allowed_schemes=[None, 'https'])
        except Exception, e:
            if str(e)\
                    != "prepend_scheme='http' is not in allowed_schemes=[None, 'https']":
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail("Accepted invalid prepend_scheme: 'http'")

        # prepend_scheme must be in allowed_schemes

        try:
            x = IS_URL(allowed_schemes=[None, 'http'],
                       prepend_scheme='https')
        except Exception, e:
            if str(e)\
                    != "prepend_scheme='https' is not in allowed_schemes=[None, 'http']":
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail("Accepted invalid prepend_scheme: 'https'")

        # prepend_scheme's value (default is 'http') must be in allowed_schemes

        try:
            x = IS_URL(mode='generic', allowed_schemes=[None, 'ftp',
                       'ftps'])
        except Exception, e:
            if str(e)\
                    != "prepend_scheme='http' is not in allowed_schemes=[None, 'ftp', 'ftps']":
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail("Accepted invalid prepend_scheme: 'http'")

        # prepend_scheme's value must be in allowed_schemes, which by default
        # is all schemes that really exist

        try:
            x = IS_URL(mode='generic', prepend_scheme='blargg')
            x('http://www.google.ca')
              # we can only reasonably know about the error at calling time
        except Exception, e:
            if not str(e).startswith(
                    "prepend_scheme='blargg' is not in allowed_schemes="):
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail("Accepted invalid prepend_scheme: 'blargg'")

        # prepend_scheme's value must be in allowed_schemes

        try:
            x = IS_URL(mode='generic', allowed_schemes=[None, 'http'],
                       prepend_scheme='blargg')
        except Exception, e:
            if str(e)\
                    != "prepend_scheme='blargg' is not in allowed_schemes=[None, 'http']":
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail("Accepted invalid prepend_scheme: 'blargg'")

        # Not inluding None in the allowed_schemes essentially disabled
        # prepending, so even though
        # prepend_scheme has the invalid value 'http', we don't care!

        x = IS_URL(allowed_schemes=['https'], prepend_scheme='https')
        self.assertEqual(x('google.ca'), ('google.ca', 'Enter a valid URL'))

        # Not inluding None in the allowed_schemes essentially disabled prepending, so even though
        # prepend_scheme has the invalid value 'http', we don't care!

        x = IS_URL(mode='generic', allowed_schemes=['https'],
                   prepend_scheme='https')
        self.assertEqual(x('google.ca'), ('google.ca', 'Enter a valid URL'))


# ##############################################################################


class TestIsGenericUrl(unittest.TestCase):

    x = IS_GENERIC_URL()

    def testInvalidUrls(self):
        urlsToCheckA = []
        for i in range(0, 32) + [127]:

            # Control characters are disallowed in any part of a URL

            urlsToCheckA.append('http://www.benn' + chr(i) + '.ca')

        urlsToCheckB = [
            None,
            '',
            'http://www.no spaces allowed.com',
            'http://www.benn.ca/no spaces allowed/',
            'http://www.benn.ca/angle_<bracket/',
            'http://www.benn.ca/angle_>bracket/',
            'http://www.benn.ca/invalid%character',
            'http://www.benn.ca/illegal%%20use',
            'http://www.benn.ca/illegaluse%',
            'http://www.benn.ca/illegaluse%0',
            'http://www.benn.ca/illegaluse%x',
            'http://www.benn.ca/ill%egaluse%x',
            'http://www.benn.ca/double"quote/',
            'http://www.curly{brace.com',
            'http://www.benn.ca/curly}brace/',
            'http://www.benn.ca/or|symbol/',
            'http://www.benn.ca/back\slash',
            'http://www.benn.ca/the^carat',
            'http://left[bracket.me',
            'http://www.benn.ca/right]bracket',
            'http://www.benn.ca/angle`quote',
            '-ttp://www.benn.ca',
            '+ttp://www.benn.ca',
            '.ttp://www.benn.ca',
            '9ttp://www.benn.ca',
            'ht;tp://www.benn.ca',
            'ht@tp://www.benn.ca',
            'ht&tp://www.benn.ca',
            'ht=tp://www.benn.ca',
            'ht$tp://www.benn.ca',
            'ht,tp://www.benn.ca',
            'ht:tp://www.benn.ca',
            'htp://invalid_scheme.com',
        ]

        failures = []

        for url in urlsToCheckA + urlsToCheckB:
            if self.x(url)[1] is None:
                failures.append('Incorrectly accepted: ' + str(url))

        if len(failures) > 0:
            self.fail(failures)

    def testValidUrls(self):
        urlsToCheck = [
            'ftp://ftp.is.co.za/rfc/rfc1808.txt',
            'gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles',
            'http://www.math.uio.no/faq/compression-faq/part1.html',
            'mailto:mduerst@ifi.unizh.ch',
            'news:comp.infosystems.www.servers.unix',
            'telnet://melvyl.ucop.edu/',
            'hTTp://www.benn.ca',
            '%66%74%70://ftp.is.co.za/rfc/rfc1808.txt',
            '%46%74%70://ftp.is.co.za/rfc/rfc1808.txt',
            '/faq/compression-faq/part1.html',
            'google.com',
            'www.google.com:8080',
            '128.127.123.250:8080',
            'blargg:ping',
            'http://www.benn.ca',
            'http://benn.ca',
            'http://amazon.com/books/',
            'https://amazon.com/movies',
            'rtsp://idontknowthisprotocol',
            'HTTP://allcaps.com',
            'http://localhost',
            'http://localhost#fragment',
            'http://localhost/hello',
            'http://localhost/hello?query=True',
            'http://localhost/hello/',
            'http://localhost:8080',
            'http://localhost:8080/',
            'http://localhost:8080/hello',
            'http://localhost:8080/hello/',
            'file:///C:/Documents%20and%20Settings/Jonathan/Desktop/view.py',
        ]

        failures = []

        for url in urlsToCheck:
            if self.x(url)[1] is not None:
                failures.append('Incorrectly rejected: ' + str(url))

        if len(failures) > 0:
            self.fail(failures)

    def testPrepending(self):
        # Does not prepend scheme for abbreviated domains
        self.assertEqual(self.x('google.ca'), ('google.ca', None))

        # Does not prepend scheme for abbreviated domains
        self.assertEqual(self.x('google.ca:8080'), ('google.ca:8080', None))

        # Does not prepend when scheme already exists
        self.assertEqual(self.x('https://google.ca'),
                         ('https://google.ca', None))

        # Does not prepend if None type is not specified in allowed_scheme,
        # because a scheme is required

        y = IS_GENERIC_URL(allowed_schemes=['http', 'blargg'],
                           prepend_scheme='http')
        self.assertEqual(y('google.ca'), ('google.ca', 'Enter a valid URL'))


# ##############################################################################


class TestIsHttpUrl(unittest.TestCase):

    x = IS_HTTP_URL()

    def testInvalidUrls(self):
        urlsToCheck = [
            None,
            '',
            'http://invalid' + chr(2) + '.com',
            'htp://invalid_scheme.com',
            'blargg://invalid_scheme.com',
            'http://-123.com',
            'http://abcd-.ca',
            'http://-abc123-.me',
            'http://www.dom&ain.com/',
            'http://www.dom=ain.com/',
            'http://www.benn.ca&',
            'http://%62%65%6E%6E%2E%63%61/path',
            'http://.domain.com',
            'http://.domain.com./path',
            'http://domain..com',
            'http://domain...at..com',
            'http://domain.com..',
            'http://domain.com../path',
            'http://domain.3m',
            'http://domain.-3m',
            'http://domain.3m-',
            'http://domain.-3m-',
            'http://domain.co&m',
            'http://domain.m3456',
            'http://domain.m-3/path#fragment',
            'http://domain.m---k/path?query=value',
            'http://23.32..',
            'http://23..32.56.0',
            'http://38997.222.999',
            'http://23.32.56.99.',
            'http://.23.32.56.99',
            'http://.23.32.56.99.',
            'http://w127.123.0.256:8080',
            'http://23.32.56.99:abcd',
            'http://23.32.56.99:23cd',
            'http://google.com:cd22',
            'http://23.32:1300.56.99',
            'http://www.yahoo:1600.com',
            'path/segment/without/starting/slash',
            'http://www.math.uio.no;param=3',
            '://ABC.com:/%7esmith/home.html',
        ]

        failures = []

        for url in urlsToCheck:
            if self.x(url)[1] is None:
                failures.append('Incorrectly accepted: ' + str(url))

        if len(failures) > 0:
            self.fail(failures)

    def testValidUrls(self):

        urlsToCheck = [
            'http://abc.com:80/~smith/home.html',
            'http://ABC.com/%7Esmith/home.html',
            'http://ABC.com:/%7esmith/home.html',
            'http://www.math.uio.no/faq/compression-faq/part1.html',
            '//google.ca/faq/compression-faq/part1.html',
            '//google.ca/faq;param=3',
            '//google.ca/faq/index.html?query=5',
            '//google.ca/faq/index.html;param=value?query=5',
            '/faq/compression-faq/part1.html',
            '/faq;param=3',
            '/faq/index.html?query=5',
            '/faq/index.html;param=value?query=5',
            'google.com',
            'benn.ca/init/default',
            'benn.ca/init;param=value/default?query=value',
            'http://host-name---with-dashes.me',
            'http://www.host-name---with-dashes.me',
            'http://a.com',
            'http://a.3.com',
            'http://a.bl-ck.com',
            'http://bl-e.b.com',
            'http://host123with456numbers.ca',
            'http://1234567890.com.',
            'http://1234567890.com./path',
            'http://google.com./path',
            'http://domain.xn--d1acj3b',
            'http://127.123.0.256',
            'http://127.123.0.256/document/drawer',
            '127.123.0.256/document/',
            '156.212.123.100',
            'http://www.google.com:180200',
            'http://www.google.com:8080/path',
            'http://www.google.com:8080',
            '//www.google.com:8080',
            'www.google.com:8080',
            'http://127.123.0.256:8080/path',
            '//127.123.0.256:8080',
            '127.123.0.256:8080',
            'http://example.me??query=value?',
            'http://a.com',
            'http://3.com',
            'http://www.benn.ca',
            'http://benn.ca',
            'http://amazon.com/books/',
            'https://amazon.com/movies',
            'hTTp://allcaps.com',
            'http://localhost',
            'HTTPS://localhost.',
            'http://localhost#fragment',
            'http://localhost/hello;param=value',
            'http://localhost/hello;param=value/hi;param2=value2;param3=value3',
            'http://localhost/hello?query=True',
            'http://www.benn.ca/hello;param=value/hi;param2=value2;param3=value3/index.html?query=3',
            'http://localhost/hello/?query=1500&five=6',
            'http://localhost:8080',
            'http://localhost:8080/',
            'http://localhost:8080/hello',
            'http://localhost:8080/hello%20world/',
            'http://www.a.3.be-nn.5.ca',
            'http://www.amazon.COM',
        ]

        failures = []

        for url in urlsToCheck:
            if self.x(url)[1] is not None:
                failures.append('Incorrectly rejected: ' + str(url))

        if len(failures) > 0:
            self.fail(failures)

    def testPrepending(self):
        # prepends scheme for abbreviated domains
        self.assertEqual(self.x('google.ca'), ('http://google.ca', None))

        # prepends scheme for abbreviated domains
        self.assertEqual(self.x('google.ca:8080'),
                         ('http://google.ca:8080', None))

        # does not prepend when scheme already exists
        self.assertEqual(self.x('https://google.ca'),
                         ('https://google.ca', None))

        y = IS_HTTP_URL(
            prepend_scheme='https', allowed_schemes=[None, 'https'])
        self.assertEqual(y('google.ca'), (
            'https://google.ca', None))  # prepends https if asked

        z = IS_HTTP_URL(prepend_scheme=None)
        self.assertEqual(z('google.ca:8080'), ('google.ca:8080',
                         None))  # prepending disabled

        try:
            IS_HTTP_URL(prepend_scheme='mailto')
        except Exception, e:
            if str(e)\
                    != "prepend_scheme='mailto' is not in allowed_schemes=[None, 'http', 'https']":
                self.fail('Wrong exception: ' + str(e))
        else:
            self.fail("Got invalid prepend_scheme: 'mailto'")

        # Does not prepend if None type is not specified in allowed_scheme, because a scheme is required

        a = IS_HTTP_URL(allowed_schemes=['http'])
        self.assertEqual(a('google.ca'), ('google.ca', 'Enter a valid URL'))
        self.assertEqual(a('google.ca:80'), ('google.ca:80',
                         'Enter a valid URL'))


class TestUnicode(unittest.TestCase):
    x = IS_URL()
    y = IS_URL(allowed_schemes=['https'], prepend_scheme='https')
               #excludes the option for abbreviated URLs with no scheme
    z = IS_URL(prepend_scheme=None)
               # disables prepending the scheme in the return value

    def testUnicodeToAsciiUrl(self):
        self.assertEquals(unicode_to_ascii_authority(u'www.Alliancefran\xe7aise.nu'), 'www.xn--alliancefranaise-npb.nu')
        self.assertEquals(
            unicode_to_ascii_authority(u'www.benn.ca'), 'www.benn.ca')
        self.assertRaises(UnicodeError, unicode_to_ascii_authority,
                          u'\u4e2d' * 1000)  # label is too long

    def testValidUrls(self):
        self.assertEquals(self.x(u'www.Alliancefrancaise.nu'), (
            'http://www.Alliancefrancaise.nu', None))
        self.assertEquals(self.x(u'www.Alliancefran\xe7aise.nu'), (
            'http://www.xn--alliancefranaise-npb.nu', None))
        self.assertEquals(self.x(u'www.Alliancefran\xe7aise.nu:8080'), (
            'http://www.xn--alliancefranaise-npb.nu:8080', None))
        self.assertEquals(self.x(u'http://www.Alliancefran\xe7aise.nu'),
                          ('http://www.xn--alliancefranaise-npb.nu', None))
        self.assertEquals(self.x(u'http://www.Alliancefran\xe7aise.nu/parnaise/blue'), ('http://www.xn--alliancefranaise-npb.nu/parnaise/blue', None))
        self.assertEquals(self.x(u'http://www.Alliancefran\xe7aise.nu/parnaise/blue#fragment'), ('http://www.xn--alliancefranaise-npb.nu/parnaise/blue#fragment', None))
        self.assertEquals(self.x(u'http://www.Alliancefran\xe7aise.nu/parnaise/blue?query=value#fragment'), ('http://www.xn--alliancefranaise-npb.nu/parnaise/blue?query=value#fragment', None))
        self.assertEquals(self.x(u'http://www.Alliancefran\xe7aise.nu:8080/parnaise/blue?query=value#fragment'), ('http://www.xn--alliancefranaise-npb.nu:8080/parnaise/blue?query=value#fragment', None))
        self.assertEquals(self.x(u'www.Alliancefran\xe7aise.nu/parnaise/blue?query=value#fragment'), ('http://www.xn--alliancefranaise-npb.nu/parnaise/blue?query=value#fragment', None))
        self.assertEquals(self.x(
            u'http://\u4e2d\u4fd4.com'), ('http://xn--fiq13b.com', None))
        self.assertEquals(self.x(u'http://\u4e2d\u4fd4.com/\u4e86'),
                          ('http://xn--fiq13b.com/%4e%86', None))
        self.assertEquals(self.x(u'http://\u4e2d\u4fd4.com/\u4e86?query=\u4e86'), ('http://xn--fiq13b.com/%4e%86?query=%4e%86', None))
        self.assertEquals(self.x(u'http://\u4e2d\u4fd4.com/\u4e86?query=\u4e86#fragment'), ('http://xn--fiq13b.com/%4e%86?query=%4e%86#fragment', None))
        self.assertEquals(self.x(u'http://\u4e2d\u4fd4.com?query=\u4e86#fragment'), ('http://xn--fiq13b.com?query=%4e%86#fragment', None))
        self.assertEquals(
            self.x(u'http://B\xfccher.ch'), ('http://xn--bcher-kva.ch', None))
        self.assertEquals(self.x(u'http://\xe4\xf6\xfc\xdf.com'), (
            'http://xn--ss-uia6e4a.com', None))
        self.assertEquals(self.x(
            u'http://visegr\xe1d.com'), ('http://xn--visegrd-mwa.com', None))
        self.assertEquals(self.x(u'http://h\xe1zipatika.com'), (
            'http://xn--hzipatika-01a.com', None))
        self.assertEquals(self.x(u'http://www.\xe7ukurova.com'), (
            'http://www.xn--ukurova-txa.com', None))
        self.assertEquals(self.x(u'http://nixier\xf6hre.nixieclock-tube.com'), ('http://xn--nixierhre-57a.nixieclock-tube.com', None))
        self.assertEquals(self.x(u'google.ca.'), ('http://google.ca.', None))

        self.assertEquals(
            self.y(u'https://google.ca'), ('https://google.ca', None))
        self.assertEquals(self.y(
            u'https://\u4e2d\u4fd4.com'), ('https://xn--fiq13b.com', None))

        self.assertEquals(self.z(u'google.ca'), ('google.ca', None))

    def testInvalidUrls(self):
        self.assertEquals(
            self.x(u'://ABC.com'), (u'://ABC.com', 'Enter a valid URL'))
        self.assertEquals(self.x(u'http://\u4e2d\u4fd4.dne'), (
            u'http://\u4e2d\u4fd4.dne', 'Enter a valid URL'))
        self.assertEquals(self.x(u'https://google.dne'), (
            u'https://google.dne', 'Enter a valid URL'))
        self.assertEquals(self.x(u'https://google..ca'), (
            u'https://google..ca', 'Enter a valid URL'))
        self.assertEquals(
            self.x(u'google..ca'), (u'google..ca', 'Enter a valid URL'))
        self.assertEquals(self.x(u'http://' + u'\u4e2d' * 1000 + u'.com'), (
            u'http://' + u'\u4e2d' * 1000 + u'.com', 'Enter a valid URL'))

        self.assertEquals(self.x(u'http://google.com#fragment_\u4e86'), (
            u'http://google.com#fragment_\u4e86', 'Enter a valid URL'))
        self.assertEquals(self.x(u'http\u4e86://google.com'), (
            u'http\u4e86://google.com', 'Enter a valid URL'))
        self.assertEquals(self.x(u'http\u4e86://google.com#fragment_\u4e86'), (
            u'http\u4e86://google.com#fragment_\u4e86', 'Enter a valid URL'))

        self.assertEquals(self.y(u'http://\u4e2d\u4fd4.com/\u4e86'), (
            u'http://\u4e2d\u4fd4.com/\u4e86', 'Enter a valid URL'))
        #self.assertEquals(self.y(u'google.ca'), (u'google.ca', 'Enter a valid URL'))

        self.assertEquals(self.z(u'invalid.domain..com'), (
            u'invalid.domain..com', 'Enter a valid URL'))
        self.assertEquals(self.z(u'invalid.\u4e2d\u4fd4.blargg'), (
            u'invalid.\u4e2d\u4fd4.blargg', 'Enter a valid URL'))

# ##############################################################################


class TestSimple(unittest.TestCase):

    def test_IS_URL(self):
        rtn = IS_URL()('abc.com')
        self.assertEqual(rtn, ('http://abc.com', None))
        rtn = IS_URL(mode='generic')('abc.com')
        self.assertEqual(rtn, ('abc.com', None))
        rtn = IS_URL(allowed_schemes=['https'], prepend_scheme='https')('https://abc.com')
        self.assertEqual(rtn, ('https://abc.com', None))
        rtn = IS_URL(prepend_scheme='https')('abc.com')
        self.assertEqual(rtn, ('https://abc.com', None))
        rtn = IS_URL(mode='generic', allowed_schemes=['ftps', 'https'], prepend_scheme='https')('https://abc.com')
        self.assertEqual(rtn, ('https://abc.com', None))
        rtn = IS_URL(mode='generic', allowed_schemes=['ftps', 'https', None], prepend_scheme='https')('abc.com')
        self.assertEqual(rtn, ('abc.com', None))
        # regression test for issue 773
        rtn = IS_URL()('domain.ninja')
        self.assertEqual(rtn, ('http://domain.ninja', None))
        # addition of allowed_tlds
        rtn = IS_URL(allowed_tlds=['com', 'net', 'org'])('domain.ninja')
        self.assertEqual(rtn, ('domain.ninja', 'Enter a valid URL'))
        # mode = 'generic' doesn't consider allowed_tlds
        rtn = IS_URL(mode='generic', allowed_tlds=['com', 'net', 'org'])('domain.ninja')
        self.assertEqual(rtn, ('domain.ninja', None))

if __name__ == '__main__':
    unittest.main()
