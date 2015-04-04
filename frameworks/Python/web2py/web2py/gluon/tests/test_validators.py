#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Unit tests for http.py """

import unittest
import datetime
import decimal
import re
from fix_path import fix_sys_path

fix_sys_path(__file__)


from gluon.validators import *


class TestValidators(unittest.TestCase):

    #port from python 2.7, needed for 2.5 and 2.6 tests
    def assertRegexpMatches(self, text, expected_regexp, msg=None):
        """Fail the test unless the text matches the regular expression."""
        if isinstance(expected_regexp, basestring):
            expected_regexp = re.compile(expected_regexp)
        if not expected_regexp.search(text):
            msg = msg or "Regexp didn't match"
            msg = '%s: %r not found in %r' % (
                msg, expected_regexp.pattern, text)
            raise self.failureException(msg)

    def test_ANY_OF(self):
        rtn = ANY_OF([IS_EMAIL(),IS_ALPHANUMERIC()])('a@b.co')
        self.assertEqual(rtn, ('a@b.co', None))
        rtn = ANY_OF([IS_EMAIL(),IS_ALPHANUMERIC()])('abco')
        self.assertEqual(rtn, ('abco', None))
        rtn = ANY_OF([IS_EMAIL(),IS_ALPHANUMERIC()])('@ab.co')
        self.assertEqual(rtn, ('@ab.co', 'Enter only letters, numbers, and underscore'))
        rtn = ANY_OF([IS_ALPHANUMERIC(),IS_EMAIL()])('@ab.co')
        self.assertEqual(rtn, ('@ab.co', 'Enter a valid email address'))

    def test_CLEANUP(self):
        rtn = CLEANUP()('helloò')
        self.assertEqual(rtn, ('hello', None))

    def test_CRYPT(self):

        rtn = str(CRYPT(digest_alg='md5',salt=True)('test')[0])
        self.assertRegexpMatches(rtn, r'^md5\$.{16}\$.{32}$')
        rtn = str(CRYPT(digest_alg='sha1',salt=True)('test')[0])
        self.assertRegexpMatches(rtn, r'^sha1\$.{16}\$.{40}$')
        rtn = str(CRYPT(digest_alg='sha256',salt=True)('test')[0])
        self.assertRegexpMatches(rtn, r'^sha256\$.{16}\$.{64}$')
        rtn = str(CRYPT(digest_alg='sha384',salt=True)('test')[0])
        self.assertRegexpMatches(rtn, r'^sha384\$.{16}\$.{96}$')
        rtn = str(CRYPT(digest_alg='sha512',salt=True)('test')[0])
        self.assertRegexpMatches(rtn, r'^sha512\$.{16}\$.{128}$')
        alg = 'pbkdf2(1000,20,sha512)'
        rtn = str(CRYPT(digest_alg=alg,salt=True)('test')[0])
        self.assertRegexpMatches(rtn, r'^pbkdf2\(1000,20,sha512\)\$.{16}\$.{40}$')
        rtn = str(CRYPT(digest_alg='md5',key='mykey',salt=True)('test')[0])
        self.assertRegexpMatches(rtn, r'^md5\$.{16}\$.{32}$')
        a = str(CRYPT(digest_alg='sha1',salt=False)('test')[0])
        self.assertEqual(CRYPT(digest_alg='sha1',salt=False)('test')[0], a)
        self.assertEqual(CRYPT(digest_alg='sha1',salt=False)('test')[0], a[6:])
        self.assertEqual(CRYPT(digest_alg='md5',salt=False)('test')[0], a)
        self.assertEqual(CRYPT(digest_alg='md5',salt=False)('test')[0], a[6:])

    def test_IS_ALPHANUMERIC(self):
        rtn = IS_ALPHANUMERIC()('1')
        self.assertEqual(rtn, ('1', None))
        rtn =  IS_ALPHANUMERIC()('')
        self.assertEqual(rtn, ('', None))
        rtn = IS_ALPHANUMERIC()('A_a')
        self.assertEqual(rtn, ('A_a', None))
        rtn = IS_ALPHANUMERIC()('!')
        self.assertEqual(rtn, ('!', 'Enter only letters, numbers, and underscore'))

    def test_IS_DATE_IN_RANGE(self):
        v = IS_DATE_IN_RANGE(minimum=datetime.date(2008,1,1),
                             maximum=datetime.date(2009,12,31),
                             format="%m/%d/%Y",error_message="oops")

        rtn = v('03/03/2008')
        self.assertEqual(rtn, (datetime.date(2008, 3, 3), None))
        rtn = v('03/03/2010')
        self.assertEqual(rtn, ('03/03/2010', 'oops'))
        rtn = v(datetime.date(2008,3,3))
        self.assertEqual(rtn, (datetime.date(2008, 3, 3), None))
        rtn =  v(datetime.date(2010,3,3))
        self.assertEqual(rtn, (datetime.date(2010, 3, 3), 'oops'))
        v = IS_DATE_IN_RANGE(maximum=datetime.date(2009,12,31),
            format="%m/%d/%Y")
        rtn = v('03/03/2010')
        self.assertEqual(rtn, ('03/03/2010', 'Enter date on or before 12/31/2009'))
        v = IS_DATE_IN_RANGE(minimum=datetime.date(2008,1,1),
            format="%m/%d/%Y")
        rtn = v('03/03/2007')
        self.assertEqual(rtn, ('03/03/2007', 'Enter date on or after 01/01/2008'))
        v = IS_DATE_IN_RANGE(minimum=datetime.date(2008,1,1),
            maximum=datetime.date(2009,12,31),
            format="%m/%d/%Y")
        rtn = v('03/03/2007')
        self.assertEqual(rtn, ('03/03/2007', 'Enter date in range 01/01/2008 12/31/2009'))

    def test_IS_DATE(self):
        v = IS_DATE(format="%m/%d/%Y",error_message="oops")
        rtn = v('03/03/2008')
        self.assertEqual(rtn, (datetime.date(2008, 3, 3), None))
        rtn = v('31/03/2008')
        self.assertEqual(rtn, ('31/03/2008', 'oops'))

    def test_IS_DATETIME_IN_RANGE(self):
        v = IS_DATETIME_IN_RANGE(
            minimum=datetime.datetime(2008,1,1,12,20),
            maximum=datetime.datetime(2009,12,31,12,20),
            format="%m/%d/%Y %H:%M",error_message="oops")
        rtn = v('03/03/2008 12:40')
        self.assertEquals(rtn, (datetime.datetime(2008, 3, 3, 12, 40), None))
        rtn = v('03/03/2010 10:34')
        self.assertEquals(rtn, ('03/03/2010 10:34', 'oops'))
        rtn =  v(datetime.datetime(2008,3,3,0,0))
        self.assertEquals(rtn, (datetime.datetime(2008, 3, 3, 0, 0), None))
        rtn =  v(datetime.datetime(2010,3,3,0,0))
        self.assertEquals(rtn, (datetime.datetime(2010, 3, 3, 0, 0), 'oops'))
        v = IS_DATETIME_IN_RANGE(maximum=datetime.datetime(2009,12,31,12,20),
            format='%m/%d/%Y %H:%M:%S')
        rtn = v('03/03/2010 12:20:00')
        self.assertEqual(rtn, ('03/03/2010 12:20:00', 'Enter date and time on or before 12/31/2009 12:20:00'))
        v = IS_DATETIME_IN_RANGE(minimum=datetime.datetime(2008,1,1,12,20),
            format='%m/%d/%Y %H:%M:%S')
        rtn = v('03/03/2007 12:20:00')
        self.assertEqual(rtn, ('03/03/2007 12:20:00', 'Enter date and time on or after 01/01/2008 12:20:00'))
        v = IS_DATETIME_IN_RANGE(minimum=datetime.datetime(2008,1,1,12,20),
            maximum=datetime.datetime(2009,12,31,12,20),
            format='%m/%d/%Y %H:%M:%S')
        rtn = v('03/03/2007 12:20:00')
        self.assertEqual(rtn, ('03/03/2007 12:20:00', 'Enter date and time in range 01/01/2008 12:20:00 12/31/2009 12:20:00'))

    def test_IS_DATETIME(self):
        v = IS_DATETIME(format="%m/%d/%Y %H:%M",error_message="oops")
        rtn = v('03/03/2008 12:40')
        self.assertEqual(rtn, (datetime.datetime(2008, 3, 3, 12, 40), None))
        rtn = v('31/03/2008 29:40')
        self.assertEqual(rtn, ('31/03/2008 29:40', 'oops'))

    def test_IS_DECIMAL_IN_RANGE(self):
        rtn =  IS_DECIMAL_IN_RANGE(1,5)('4')
        self.assertEqual(rtn, (decimal.Decimal('4'), None))
        rtn =  IS_DECIMAL_IN_RANGE(1,5)(4)
        self.assertEqual(rtn, (decimal.Decimal('4'), None))
        rtn =  IS_DECIMAL_IN_RANGE(1,5)(1)
        self.assertEqual(rtn, (decimal.Decimal('1'), None))
        rtn =  IS_DECIMAL_IN_RANGE(1,5)(5.25)
        self.assertEqual(rtn, (5.25, 'Enter a number between 1 and 5'))
        rtn =  IS_DECIMAL_IN_RANGE(5.25,6)(5.25)
        self.assertEqual(rtn, (decimal.Decimal('5.25'), None))
        rtn =  IS_DECIMAL_IN_RANGE(5.25,6)('5.25')
        self.assertEqual(rtn, (decimal.Decimal('5.25'), None))
        rtn =  IS_DECIMAL_IN_RANGE(1,5)(6.0)
        self.assertEqual(rtn, (6.0, 'Enter a number between 1 and 5'))
        rtn =  IS_DECIMAL_IN_RANGE(1,5)(3.5)
        self.assertEqual(rtn, (decimal.Decimal('3.5'), None))
        rtn =  IS_DECIMAL_IN_RANGE(1.5,5.5)(3.5)
        self.assertEqual(rtn, (decimal.Decimal('3.5'), None))
        rtn =  IS_DECIMAL_IN_RANGE(1.5,5.5)(6.5)
        self.assertEqual(rtn, (6.5, 'Enter a number between 1.5 and 5.5'))
        rtn =  IS_DECIMAL_IN_RANGE(1.5,None)(6.5)
        self.assertEqual(rtn, (decimal.Decimal('6.5'), None))
        rtn =  IS_DECIMAL_IN_RANGE(1.5,None)(0.5)
        self.assertEqual(rtn, (0.5, 'Enter a number greater than or equal to 1.5'))
        rtn =  IS_DECIMAL_IN_RANGE(None,5.5)(4.5)
        self.assertEqual(rtn, (decimal.Decimal('4.5'), None))
        rtn =  IS_DECIMAL_IN_RANGE(None,5.5)(6.5)
        self.assertEqual(rtn, (6.5, 'Enter a number less than or equal to 5.5'))
        rtn =  IS_DECIMAL_IN_RANGE()(6.5)
        self.assertEqual(rtn, (decimal.Decimal('6.5'), None))
        rtn =  IS_DECIMAL_IN_RANGE(0,99)(123.123)
        self.assertEqual(rtn, (123.123, 'Enter a number between 0 and 99'))
        rtn =  IS_DECIMAL_IN_RANGE(0,99)('123.123')
        self.assertEqual(rtn, ('123.123', 'Enter a number between 0 and 99'))
        rtn =  IS_DECIMAL_IN_RANGE(0,99)('12.34')
        self.assertEqual(rtn, (decimal.Decimal('12.34'), None))
        rtn =  IS_DECIMAL_IN_RANGE()('abc')
        self.assertEqual(rtn, ('abc', 'Enter a number'))
        rtn = IS_DECIMAL_IN_RANGE()('6,5')
        self.assertEqual(rtn, ('6,5', 'Enter a number'))
        rtn = IS_DECIMAL_IN_RANGE(dot=',')('6.5')
        self.assertEqual(rtn, (decimal.Decimal('6.5'), None))
        rtn =  IS_DECIMAL_IN_RANGE(1,5)(decimal.Decimal('4'))
        self.assertEqual(rtn, (decimal.Decimal('4'), None))

    def test_IS_EMAIL(self):
        rtn =  IS_EMAIL()('a@b.com')
        self.assertEqual(rtn, ('a@b.com', None))
        rtn =  IS_EMAIL()('abc@def.com')
        self.assertEqual(rtn, ('abc@def.com', None))
        rtn =  IS_EMAIL()('abc@3def.com')
        self.assertEqual(rtn, ('abc@3def.com', None))
        rtn =  IS_EMAIL()('abc@def.us')
        self.assertEqual(rtn, ('abc@def.us', None))
        rtn =  IS_EMAIL()('abc@d_-f.us')
        self.assertEqual(rtn, ('abc@d_-f.us', None))
        rtn =  IS_EMAIL()('@def.com')           # missing name
        self.assertEqual(rtn, ('@def.com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('"abc@def".com')      # quoted name
        self.assertEqual(rtn, ('"abc@def".com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('abc+def.com')        # no @
        self.assertEqual(rtn, ('abc+def.com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('abc@def.x')          # one-char TLD
        self.assertEqual(rtn, ('abc@def.x', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('abc@def.12')         # numeric TLD
        self.assertEqual(rtn, ('abc@def.12', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('abc@def..com')       # double-dot in domain
        self.assertEqual(rtn, ('abc@def..com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('abc@.def.com')       # dot starts domain
        self.assertEqual(rtn, ('abc@.def.com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('abc@def.c_m')        # underscore in TLD
        self.assertEqual(rtn, ('abc@def.c_m', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('NotAnEmail')         # missing @
        self.assertEqual(rtn, ('NotAnEmail', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('abc@NotAnEmail')     # missing TLD
        self.assertEqual(rtn, ('abc@NotAnEmail', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('customer/department@example.com')
        self.assertEqual(rtn, ('customer/department@example.com', None))
        rtn =  IS_EMAIL()('$A12345@example.com')
        self.assertEqual(rtn, ('$A12345@example.com', None))
        rtn =  IS_EMAIL()('!def!xyz%abc@example.com')
        self.assertEqual(rtn, ('!def!xyz%abc@example.com', None))
        rtn =  IS_EMAIL()('_Yosemite.Sam@example.com')
        self.assertEqual(rtn, ('_Yosemite.Sam@example.com', None))
        rtn =  IS_EMAIL()('~@example.com')
        self.assertEqual(rtn, ('~@example.com', None))
        rtn =  IS_EMAIL()('.wooly@example.com')       # dot starts name
        self.assertEqual(rtn, ('.wooly@example.com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('wo..oly@example.com')      # adjacent dots in name
        self.assertEqual(rtn, ('wo..oly@example.com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('pootietang.@example.com')  # dot ends name
        self.assertEqual(rtn, ('pootietang.@example.com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('.@example.com')            # name is bare dot
        self.assertEqual(rtn, ('.@example.com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('Ima.Fool@example.com')
        self.assertEqual(rtn, ('Ima.Fool@example.com', None))
        rtn =  IS_EMAIL()('Ima Fool@example.com')     # space in name
        self.assertEqual(rtn, ('Ima Fool@example.com', 'Enter a valid email address'))
        rtn =  IS_EMAIL()('localguy@localhost')       # localhost as domain
        self.assertEqual(rtn, ('localguy@localhost', None))
        # test for banned
        rtn =  IS_EMAIL(banned='^.*\.com(|\..*)$')('localguy@localhost')       # localhost as domain
        self.assertEqual(rtn, ('localguy@localhost', None))
        rtn = IS_EMAIL(banned='^.*\.com(|\..*)$')('abc@example.com')
        self.assertEqual(rtn, ('abc@example.com', 'Enter a valid email address'))
        # test for forced
        rtn = IS_EMAIL(forced='^.*\.edu(|\..*)$')('localguy@localhost')
        self.assertEqual(rtn, ('localguy@localhost', 'Enter a valid email address'))
        rtn = IS_EMAIL(forced='^.*\.edu(|\..*)$')('localguy@example.edu')
        self.assertEqual(rtn, ('localguy@example.edu', None))

    def test_IS_LIST_OF_EMAILS(self):
        emails = ['localguy@localhost', '_Yosemite.Sam@example.com']
        rtn = IS_LIST_OF_EMAILS()(','.join(emails))
        self.assertEqual(rtn, (','.join(emails), None))
        rtn = IS_LIST_OF_EMAILS()(';'.join(emails))
        self.assertEqual(rtn, (';'.join(emails), None))
        rtn = IS_LIST_OF_EMAILS()(' '.join(emails))
        self.assertEqual(rtn, (' '.join(emails), None))
        emails.append('a')
        rtn = IS_LIST_OF_EMAILS()(';'.join(emails))
        self.assertEqual(rtn, ('localguy@localhost;_Yosemite.Sam@example.com;a', 'Invalid emails: a'))

    def test_IS_LIST_OF(self):
        values = [0,1,2,3,4]
        rtn = IS_LIST_OF(IS_INT_IN_RANGE(0, 10))(values)
        self.assertEqual(rtn, (values, None))
        values.append(11)
        rtn = IS_LIST_OF(IS_INT_IN_RANGE(0, 10))(values)
        self.assertEqual(rtn, (values, 'Enter an integer between 0 and 9'))
        rtn = IS_LIST_OF(IS_INT_IN_RANGE(0, 10))(1)
        self.assertEqual(rtn, ([1], None))
        rtn = IS_LIST_OF(IS_INT_IN_RANGE(0, 10), minimum=10)([1,2])
        self.assertEqual(rtn, ([1, 2], 'Enter between 10 and 100 values'))
        rtn =  IS_LIST_OF(IS_INT_IN_RANGE(0, 10), maximum=2)([1,2,3])
        self.assertEqual(rtn, ([1, 2, 3], 'Enter between 0 and 2 values'))
        # regression test for issue 742
        rtn = IS_LIST_OF(minimum=1)('')
        self.assertEqual(rtn, ([], 'Enter between 1 and 100 values'))


    def test_IS_EMPTY_OR(self):
        rtn = IS_EMPTY_OR(IS_EMAIL())('abc@def.com')
        self.assertEqual(rtn, ('abc@def.com', None))
        rtn =  IS_EMPTY_OR(IS_EMAIL())('   ')
        self.assertEqual(rtn, (None, None))
        rtn =  IS_EMPTY_OR(IS_EMAIL(), null='abc')('   ')
        self.assertEqual(rtn, ('abc', None))
        rtn =  IS_EMPTY_OR(IS_EMAIL(), null='abc', empty_regex='def')('def')
        self.assertEqual(rtn, ('abc', None))
        rtn =  IS_EMPTY_OR(IS_EMAIL())('abc')
        self.assertEqual(rtn, ('abc', 'Enter a valid email address'))
        rtn =  IS_EMPTY_OR(IS_EMAIL())(' abc ')
        self.assertEqual(rtn, ('abc', 'Enter a valid email address'))

    def test_IS_EXPR(self):
        rtn = IS_EXPR('int(value) < 2')('1')
        self.assertEqual(rtn, ('1', None))
        rtn = IS_EXPR('int(value) < 2')('2')
        self.assertEqual(rtn, ('2', 'Invalid expression'))
        rtn = IS_EXPR(lambda value: int(value))('1')
        self.assertEqual(rtn, ('1', 1))
        rtn = IS_EXPR(lambda value: int(value) < 2 and 'invalid' or None)('2')
        self.assertEqual(rtn, ('2', None))

    def test_IS_FLOAT_IN_RANGE(self):
        rtn = IS_FLOAT_IN_RANGE(1,5)('4')
        self.assertEqual(rtn, (4.0, None))
        rtn = IS_FLOAT_IN_RANGE(1,5)(4)
        self.assertEqual(rtn, (4.0, None))
        rtn = IS_FLOAT_IN_RANGE(1,5)(1)
        self.assertEqual(rtn, (1.0, None))
        rtn = IS_FLOAT_IN_RANGE(1,5)(5.25)
        self.assertEqual(rtn, (5.25, 'Enter a number between 1 and 5'))
        rtn = IS_FLOAT_IN_RANGE(1,5)(6.0)
        self.assertEqual(rtn, (6.0, 'Enter a number between 1 and 5'))
        rtn = IS_FLOAT_IN_RANGE(1,5)(3.5)
        self.assertEqual(rtn, (3.5, None))
        rtn = IS_FLOAT_IN_RANGE(1,None)(3.5)
        self.assertEqual(rtn, (3.5, None))
        rtn = IS_FLOAT_IN_RANGE(None,5)(3.5)
        self.assertEqual(rtn, (3.5, None))
        rtn = IS_FLOAT_IN_RANGE(1,None)(0.5)
        self.assertEqual(rtn, (0.5, 'Enter a number greater than or equal to 1'))
        rtn = IS_FLOAT_IN_RANGE(None,5)(6.5)
        self.assertEqual(rtn, (6.5, 'Enter a number less than or equal to 5'))
        rtn = IS_FLOAT_IN_RANGE()(6.5)
        self.assertEqual(rtn, (6.5, None))
        rtn = IS_FLOAT_IN_RANGE()('abc')
        self.assertEqual(rtn, ('abc', 'Enter a number'))
        rtn = IS_FLOAT_IN_RANGE()('6,5')
        self.assertEqual(rtn, ('6,5', 'Enter a number'))
        rtn = IS_FLOAT_IN_RANGE(dot=',')('6.5')
        self.assertEqual(rtn, (6.5, None))


    def test_IS_IN_SET(self):
        rtn = IS_IN_SET(['max', 'john'])('max')
        self.assertEqual(rtn, ('max', None))
        rtn = IS_IN_SET(['max', 'john'])('massimo')
        self.assertEqual(rtn, ('massimo', 'Value not allowed'))
        rtn = IS_IN_SET(['max', 'john'], multiple=True)(('max', 'john'))
        self.assertEqual(rtn, (('max', 'john'), None))
        rtn = IS_IN_SET(['max', 'john'], multiple=True)(('bill', 'john'))
        self.assertEqual(rtn, (('bill', 'john'), 'Value not allowed'))
        rtn = IS_IN_SET(('id1','id2'), ['first label','second label'])('id1') # Traditional way
        self.assertEqual(rtn, ('id1', None))
        rtn = IS_IN_SET({'id1':'first label', 'id2':'second label'})('id1')
        self.assertEqual(rtn, ('id1', None))
        import itertools
        rtn = IS_IN_SET(itertools.chain(['1','3','5'],['2','4','6']))('1')
        self.assertEqual(rtn, ('1', None))
        rtn = IS_IN_SET([('id1','first label'), ('id2','second label')])('id1') # Redundant way
        self.assertEqual(rtn, ('id1', None))

    def test_IS_INT_IN_RANGE(self):
        rtn =  IS_INT_IN_RANGE(1,5)('4')
        self.assertEqual(rtn, (4, None))
        rtn =  IS_INT_IN_RANGE(1,5)(4)
        self.assertEqual(rtn, (4, None))
        rtn =  IS_INT_IN_RANGE(1,5)(1)
        self.assertEqual(rtn, (1, None))
        rtn =  IS_INT_IN_RANGE(1,5)(5)
        self.assertEqual(rtn, (5, 'Enter an integer between 1 and 4'))
        rtn =  IS_INT_IN_RANGE(1,5)(5)
        self.assertEqual(rtn, (5, 'Enter an integer between 1 and 4'))
        rtn =  IS_INT_IN_RANGE(1,5)(3.5)
        self.assertEqual(rtn, (3.5, 'Enter an integer between 1 and 4'))
        rtn =  IS_INT_IN_RANGE(None,5)('4')
        self.assertEqual(rtn, (4, None))
        rtn =  IS_INT_IN_RANGE(None,5)('6')
        self.assertEqual(rtn, ('6', 'Enter an integer less than or equal to 4'))
        rtn =  IS_INT_IN_RANGE(1,None)('4')
        self.assertEqual(rtn, (4, None))
        rtn =  IS_INT_IN_RANGE(1,None)('0')
        self.assertEqual(rtn, ('0', 'Enter an integer greater than or equal to 1'))
        rtn =  IS_INT_IN_RANGE()(6)
        self.assertEqual(rtn, (6, None))
        rtn =  IS_INT_IN_RANGE()('abc')
        self.assertEqual(rtn, ('abc', 'Enter an integer'))

    def test_IS_IPV4(self):
        rtn = IS_IPV4()('1.2.3.4')
        self.assertEqual(rtn, ('1.2.3.4', None))
        rtn = IS_IPV4()('255.255.255.255')
        self.assertEqual(rtn, ('255.255.255.255', None))
        rtn = IS_IPV4()('1.2.3.4 ')
        self.assertEqual(rtn, ('1.2.3.4 ', 'Enter valid IPv4 address'))
        rtn = IS_IPV4()('1.2.3.4.5')
        self.assertEqual(rtn, ('1.2.3.4.5', 'Enter valid IPv4 address'))
        rtn = IS_IPV4()('123.123')
        self.assertEqual(rtn, ('123.123', 'Enter valid IPv4 address'))
        rtn = IS_IPV4()('1111.2.3.4')
        self.assertEqual(rtn, ('1111.2.3.4', 'Enter valid IPv4 address'))
        rtn = IS_IPV4()('0111.2.3.4')
        self.assertEqual(rtn, ('0111.2.3.4', 'Enter valid IPv4 address'))
        rtn = IS_IPV4()('256.2.3.4')
        self.assertEqual(rtn, ('256.2.3.4', 'Enter valid IPv4 address'))
        rtn = IS_IPV4()('300.2.3.4')
        self.assertEqual(rtn, ('300.2.3.4', 'Enter valid IPv4 address'))
        rtn = IS_IPV4(minip='1.2.3.4', maxip='1.2.3.4')('1.2.3.4')
        self.assertEqual(rtn, ('1.2.3.4', None))
        rtn = IS_IPV4(minip='1.2.3.5', maxip='1.2.3.9', error_message='bad ip')('1.2.3.4')
        self.assertEqual(rtn, ('1.2.3.4', 'bad ip'))
        rtn = IS_IPV4(maxip='1.2.3.4', invert=True)('127.0.0.1')
        self.assertEqual(rtn, ('127.0.0.1', None))
        rtn = IS_IPV4(maxip='1.2.3.4', invert=True)('1.2.3.4')
        self.assertEqual(rtn, ('1.2.3.4', 'Enter valid IPv4 address'))
        rtn = IS_IPV4(is_localhost=True)('127.0.0.1')
        self.assertEqual(rtn, ('127.0.0.1', None))
        rtn = IS_IPV4(is_localhost=True)('1.2.3.4')
        self.assertEqual(rtn, ('1.2.3.4', 'Enter valid IPv4 address'))
        rtn = IS_IPV4(is_localhost=False)('127.0.0.1')
        self.assertEqual(rtn, ('127.0.0.1', 'Enter valid IPv4 address'))
        rtn = IS_IPV4(maxip='100.0.0.0', is_localhost=True)('127.0.0.1')
        self.assertEqual(rtn, ('127.0.0.1', 'Enter valid IPv4 address'))

    def test_IS_IPV6(self):
        rtn = IS_IPV6()('fe80::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPV6()('192.168.1.1')
        self.assertEqual(rtn, ('192.168.1.1', 'Enter valid IPv6 address'))
        rtn = IS_IPV6(error_message='bad ip')('192.168.1.1')
        self.assertEqual(rtn, ('192.168.1.1', 'bad ip'))
        rtn = IS_IPV6(is_link_local=True)('fe80::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPV6(is_link_local=False)('fe80::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af', 'Enter valid IPv6 address'))
        rtn = IS_IPV6(is_link_local=True)('2001::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::126c:8ffa:fe22:b3af', 'Enter valid IPv6 address'))
        rtn = IS_IPV6(is_multicast=True)('2001::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::126c:8ffa:fe22:b3af', 'Enter valid IPv6 address'))
        rtn = IS_IPV6(is_multicast=True)('ff00::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('ff00::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPV6(is_routeable=True)('2001::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPV6(is_routeable=True)('ff00::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('ff00::126c:8ffa:fe22:b3af', 'Enter valid IPv6 address'))
        rtn = IS_IPV6(subnets='2001::/32')('2001::8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::8ffa:fe22:b3af', None))
        rtn = IS_IPV6(subnets='fb00::/8')('2001::8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::8ffa:fe22:b3af', 'Enter valid IPv6 address'))
        rtn = IS_IPV6(subnets=['fc00::/8','2001::/32'])('2001::8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::8ffa:fe22:b3af', None))
        rtn = IS_IPV6(subnets='invalidsubnet')('2001::8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::8ffa:fe22:b3af', 'invalid subnet provided'))

    def test_IS_IPADDRESS(self):
        rtn = IS_IPADDRESS()('192.168.1.5')
        self.assertEqual(rtn, ('192.168.1.5', None))
        rtn = IS_IPADDRESS(is_ipv6=False)('192.168.1.5')
        self.assertEqual(rtn, ('192.168.1.5', None))
        rtn = IS_IPADDRESS()('255.255.255.255')
        self.assertEqual(rtn, ('255.255.255.255', None))
        rtn = IS_IPADDRESS()('192.168.1.5 ')
        self.assertEqual(rtn, ('192.168.1.5 ', 'Enter valid IP address'))
        rtn = IS_IPADDRESS()('192.168.1.1.5')
        self.assertEqual(rtn, ('192.168.1.1.5', 'Enter valid IP address'))
        rtn = IS_IPADDRESS()('123.123')
        self.assertEqual(rtn, ('123.123', 'Enter valid IP address'))
        rtn = IS_IPADDRESS()('1111.2.3.4')
        self.assertEqual(rtn, ('1111.2.3.4', 'Enter valid IP address'))
        rtn = IS_IPADDRESS()('0111.2.3.4')
        self.assertEqual(rtn, ('0111.2.3.4', 'Enter valid IP address'))
        rtn = IS_IPADDRESS()('256.2.3.4')
        self.assertEqual(rtn, ('256.2.3.4', 'Enter valid IP address'))
        rtn = IS_IPADDRESS()('300.2.3.4')
        self.assertEqual(rtn, ('300.2.3.4', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(minip='192.168.1.0', maxip='192.168.1.255')('192.168.1.100')
        self.assertEqual(rtn, ('192.168.1.100', None))
        rtn = IS_IPADDRESS(minip='1.2.3.5', maxip='1.2.3.9', error_message='bad ip')('1.2.3.4')
        self.assertEqual(rtn, ('1.2.3.4', 'bad ip'))
        rtn = IS_IPADDRESS(maxip='1.2.3.4', invert=True)('127.0.0.1')
        self.assertEqual(rtn, ('127.0.0.1', None))
        rtn = IS_IPADDRESS(maxip='192.168.1.4', invert=True)('192.168.1.4')
        self.assertEqual(rtn, ('192.168.1.4', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(is_localhost=True)('127.0.0.1')
        self.assertEqual(rtn, ('127.0.0.1', None))
        rtn = IS_IPADDRESS(is_localhost=True)('192.168.1.10')
        self.assertEqual(rtn, ('192.168.1.10', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(is_localhost=False)('127.0.0.1')
        self.assertEqual(rtn, ('127.0.0.1', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(maxip='100.0.0.0', is_localhost=True)('127.0.0.1')
        self.assertEqual(rtn, ('127.0.0.1', 'Enter valid IP address'))
        rtn = IS_IPADDRESS()('aaa')
        self.assertEqual(rtn, ('aaa', 'Enter valid IP address'))


        rtn = IS_IPADDRESS()('fe80::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPADDRESS(is_ipv4=False)('fe80::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPADDRESS()('fe80::126c:8ffa:fe22:b3af  ')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af  ', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(is_ipv4=True)('fe80::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(is_ipv6=True)('192.168.1.1')
        self.assertEqual(rtn, ('192.168.1.1', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(is_ipv6=True, error_message='bad ip')('192.168.1.1')
        self.assertEqual(rtn, ('192.168.1.1', 'bad ip'))
        rtn = IS_IPADDRESS(is_link_local=True)('fe80::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPADDRESS(is_link_local=False)('fe80::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('fe80::126c:8ffa:fe22:b3af', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(is_link_local=True)('2001::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::126c:8ffa:fe22:b3af', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(is_multicast=True)('2001::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::126c:8ffa:fe22:b3af', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(is_multicast=True)('ff00::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('ff00::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPADDRESS(is_routeable=True)('2001::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::126c:8ffa:fe22:b3af', None))
        rtn = IS_IPADDRESS(is_routeable=True)('ff00::126c:8ffa:fe22:b3af')
        self.assertEqual(rtn, ('ff00::126c:8ffa:fe22:b3af', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(subnets='2001::/32')('2001::8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::8ffa:fe22:b3af', None))
        rtn = IS_IPADDRESS(subnets='fb00::/8')('2001::8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::8ffa:fe22:b3af', 'Enter valid IP address'))
        rtn = IS_IPADDRESS(subnets=['fc00::/8','2001::/32'])('2001::8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::8ffa:fe22:b3af', None))
        rtn = IS_IPADDRESS(subnets='invalidsubnet')('2001::8ffa:fe22:b3af')
        self.assertEqual(rtn, ('2001::8ffa:fe22:b3af', 'invalid subnet provided'))

    def test_IS_LENGTH(self):
        rtn = IS_LENGTH()('')
        self.assertEqual(rtn, ('', None))
        rtn = IS_LENGTH()('1234567890')
        self.assertEqual(rtn, ('1234567890', None))
        rtn = IS_LENGTH(maxsize=5, minsize=0)('1234567890')  # too long
        self.assertEqual(rtn, ('1234567890', 'Enter from 0 to 5 characters'))
        rtn = IS_LENGTH(maxsize=50, minsize=20)('1234567890')  # too short
        self.assertEqual(rtn, ('1234567890', 'Enter from 20 to 50 characters'))
        rtn = IS_LENGTH()(None)
        self.assertEqual(rtn, (None, None))
        rtn = IS_LENGTH(minsize=0)(None)
        self.assertEqual(rtn, (None, None))
        rtn = IS_LENGTH(minsize=1)(None)
        self.assertEqual(rtn, (None, 'Enter from 1 to 255 characters'))
        rtn = IS_LENGTH(minsize=1)([])
        self.assertEqual(rtn, ([], 'Enter from 1 to 255 characters'))
        rtn = IS_LENGTH(minsize=1)([1, 2])
        self.assertEqual(rtn, ([1, 2], None))
        rtn = IS_LENGTH(minsize=1)([1])
        self.assertEqual(rtn, ([1], None))
        # test unicode
        rtn = IS_LENGTH(2)(u'°2')
        self.assertEqual(rtn, ('\xc2\xb02', None))
        rtn = IS_LENGTH(2)(u'°12')
        self.assertEqual(rtn, (u'\xb012', 'Enter from 0 to 2 characters'))
        # test automatic str()
        rtn = IS_LENGTH(minsize=1)(1)
        self.assertEqual(rtn, ('1', None))
        rtn = IS_LENGTH(minsize=2)(1)
        self.assertEqual(rtn, (1, 'Enter from 2 to 255 characters'))
        # test FieldStorage
        import cgi
        from StringIO import StringIO
        a = cgi.FieldStorage()
        a.file = StringIO('abc')
        rtn = IS_LENGTH(minsize=4)(a)
        self.assertEqual(rtn, (a, 'Enter from 4 to 255 characters'))
        urlencode_data = "key2=value2x&key3=value3&key4=value4"
        urlencode_environ = {
            'CONTENT_LENGTH':   str(len(urlencode_data)),
            'CONTENT_TYPE':     'application/x-www-form-urlencoded',
            'QUERY_STRING':     'key1=value1&key2=value2y',
            'REQUEST_METHOD':   'POST',
        }
        fake_stdin = StringIO(urlencode_data)
        fake_stdin.seek(0)
        a = cgi.FieldStorage(fp=fake_stdin, environ=urlencode_environ)
        rtn = IS_LENGTH(minsize=6)(a)
        self.assertEqual(rtn, (a, 'Enter from 6 to 255 characters'))
        a = cgi.FieldStorage()
        rtn = IS_LENGTH(minsize=6)(a)
        self.assertEqual(rtn, (a, 'Enter from 6 to 255 characters'))
        rtn = IS_LENGTH(6)(a)
        self.assertEqual(rtn, (a, None))

    def test_IS_LOWER(self):
        rtn = IS_LOWER()('ABC')
        self.assertEqual(rtn, ('abc', None))
        rtn = IS_LOWER()('Ñ')
        self.assertEqual(rtn, ('\xc3\xb1', None))

    def test_IS_MATCH(self):
        rtn = IS_MATCH('.+')('hello')
        self.assertEqual(rtn, ('hello', None))
        rtn = IS_MATCH('hell')('hello')
        self.assertEqual(rtn, ('hello', None))
        rtn = IS_MATCH('hell.*', strict=False)('hello')
        self.assertEqual(rtn, ('hello', None))
        rtn = IS_MATCH('hello')('shello')
        self.assertEqual(rtn, ('shello', 'Invalid expression'))
        rtn = IS_MATCH('hello', search=True)('shello')
        self.assertEqual(rtn, ('shello', None))
        rtn = IS_MATCH('hello', search=True, strict=False)('shellox')
        self.assertEqual(rtn, ('shellox', None))
        rtn = IS_MATCH('.*hello.*', search=True, strict=False)('shellox')
        self.assertEqual(rtn, ('shellox', None))
        rtn = IS_MATCH('.+')('')
        self.assertEqual(rtn, ('', 'Invalid expression'))
        rtn = IS_MATCH('hell', strict=True)('hellas')
        self.assertEqual(rtn, ('hellas', 'Invalid expression'))
        rtn = IS_MATCH('hell$', strict=True)('hellas')
        self.assertEqual(rtn, ('hellas', 'Invalid expression'))
        rtn = IS_MATCH('^.hell$', strict=True)('shell')
        self.assertEqual(rtn, ('shell', None))
        rtn = IS_MATCH(u'hell', is_unicode=True)('àòè')
        self.assertEqual(rtn, ('\xc3\xa0\xc3\xb2\xc3\xa8', 'Invalid expression'))
        rtn = IS_MATCH(u'hell', is_unicode=True)(u'hell')
        self.assertEqual(rtn, (u'hell', None))
        rtn = IS_MATCH('hell', is_unicode=True)(u'hell')
        self.assertEqual(rtn, (u'hell', None))


    def test_IS_EQUAL_TO(self):
        rtn = IS_EQUAL_TO('aaa')('aaa')
        self.assertEqual(rtn, ('aaa', None))
        rtn = IS_EQUAL_TO('aaa')('aab')
        self.assertEqual(rtn, ('aab', 'No match'))

    def test_IS_NOT_EMPTY(self):
        rtn = IS_NOT_EMPTY()(1)
        self.assertEqual(rtn, (1, None))
        rtn = IS_NOT_EMPTY()(0)
        self.assertEqual(rtn, (0, None))
        rtn = IS_NOT_EMPTY()('x')
        self.assertEqual(rtn, ('x', None))
        rtn = IS_NOT_EMPTY()(' x ')
        self.assertEqual(rtn, ('x', None))
        rtn = IS_NOT_EMPTY()(None)
        self.assertEqual(rtn, (None, 'Enter a value'))
        rtn = IS_NOT_EMPTY()('')
        self.assertEqual(rtn, ('', 'Enter a value'))
        rtn = IS_NOT_EMPTY()('  ')
        self.assertEqual(rtn, ('', 'Enter a value'))
        rtn = IS_NOT_EMPTY()(' \n\t')
        self.assertEqual(rtn, ('', 'Enter a value'))
        rtn = IS_NOT_EMPTY()([])
        self.assertEqual(rtn, ([], 'Enter a value'))
        rtn = IS_NOT_EMPTY(empty_regex='def')('def')
        self.assertEqual(rtn, ('', 'Enter a value'))
        rtn = IS_NOT_EMPTY(empty_regex='de[fg]')('deg')
        self.assertEqual(rtn, ('', 'Enter a value'))
        rtn = IS_NOT_EMPTY(empty_regex='def')('abc')
        self.assertEqual(rtn, ('abc', None))

    def test_IS_SLUG(self):
        rtn = IS_SLUG()('abc123')
        self.assertEqual(rtn, ('abc123', None))
        rtn = IS_SLUG()('ABC123')
        self.assertEqual(rtn, ('abc123', None))
        rtn = IS_SLUG()('abc-123')
        self.assertEqual(rtn, ('abc-123', None))
        rtn = IS_SLUG()('abc--123')
        self.assertEqual(rtn, ('abc-123', None))
        rtn = IS_SLUG()('abc 123')
        self.assertEqual(rtn, ('abc-123', None))
        rtn = IS_SLUG()('abc\t_123')
        self.assertEqual(rtn, ('abc-123', None))
        rtn = IS_SLUG()('-abc-')
        self.assertEqual(rtn, ('abc', None))
        rtn = IS_SLUG()('--a--b--_ -c--')
        self.assertEqual(rtn, ('a-b-c', None))
        rtn = IS_SLUG()('abc&amp;123')
        self.assertEqual(rtn, ('abc123', None))
        rtn = IS_SLUG()('abc&amp;123&amp;def')
        self.assertEqual(rtn, ('abc123def', None))
        rtn = IS_SLUG()('ñ')
        self.assertEqual(rtn, ('n', None))
        rtn = IS_SLUG(maxlen=4)('abc123')
        self.assertEqual(rtn, ('abc1', None))
        rtn = IS_SLUG()('abc_123')
        self.assertEqual(rtn, ('abc-123', None))
        rtn = IS_SLUG(keep_underscores=False)('abc_123')
        self.assertEqual(rtn, ('abc-123', None))
        rtn = IS_SLUG(keep_underscores=True)('abc_123')
        self.assertEqual(rtn, ('abc_123', None))
        rtn = IS_SLUG(check=False)('abc')
        self.assertEqual(rtn, ('abc', None))
        rtn = IS_SLUG(check=True)('abc')
        self.assertEqual(rtn, ('abc', None))
        rtn = IS_SLUG(check=False)('a bc')
        self.assertEqual(rtn, ('a-bc', None))
        rtn = IS_SLUG(check=True)('a bc')
        self.assertEqual(rtn, ('a bc', 'Must be slug'))

    def test_IS_STRONG(self):
        rtn = IS_STRONG(es=True)('Abcd1234')
        self.assertEqual(rtn, ('Abcd1234',
         'Must include at least 1 of the following: ~!@#$%^&*()_+-=?<>,.:;{}[]|'))
        rtn = IS_STRONG(es=True)('Abcd1234!')
        self.assertEqual(rtn, ('Abcd1234!', None))
        rtn = IS_STRONG(es=True, entropy=1)('a')
        self.assertEqual(rtn, ('a', None))
        rtn = IS_STRONG(es=True, entropy=1, min=2)('a')
        self.assertEqual(rtn, ('a', 'Minimum length is 2'))
        rtn = IS_STRONG(es=True, entropy=100)('abc123')
        self.assertEqual(rtn, ('abc123', 'Entropy (32.35) less than required (100)'))
        rtn = IS_STRONG(es=True, entropy=100)('and')
        self.assertEqual(rtn, ('and', 'Entropy (14.57) less than required (100)'))
        rtn = IS_STRONG(es=True, entropy=100)('aaa')
        self.assertEqual(rtn, ('aaa', 'Entropy (14.42) less than required (100)'))
        rtn = IS_STRONG(es=True, entropy=100)('a1d')
        self.assertEqual(rtn, ('a1d', 'Entropy (15.97) less than required (100)'))
        rtn = IS_STRONG(es=True, entropy=100)('añd')
        self.assertEqual(rtn, ('a\xc3\xb1d', 'Entropy (18.13) less than required (100)'))
        rtn = IS_STRONG()('********')
        self.assertEqual(rtn, ('********', None))
        rtn = IS_STRONG(es=True, max=4)('abcde')
        self.assertEqual(rtn,
            ('abcde',
             '|'.join(['Minimum length is 8',
                      'Maximum length is 4',
                      'Must include at least 1 of the following: ~!@#$%^&*()_+-=?<>,.:;{}[]|',
                      'Must include at least 1 upper case',
                      'Must include at least 1 number']))
            )
        rtn = IS_STRONG(es=True)('abcde')
        self.assertEqual(rtn,
            ('abcde',
             '|'.join(['Minimum length is 8',
                      'Must include at least 1 of the following: ~!@#$%^&*()_+-=?<>,.:;{}[]|',
                      'Must include at least 1 upper case',
                      'Must include at least 1 number']))
            )


    def test_IS_TIME(self):
        rtn = IS_TIME()('21:30')
        self.assertEqual(rtn, (datetime.time(21, 30), None))
        rtn = IS_TIME()('21-30')
        self.assertEqual(rtn, (datetime.time(21, 30), None))
        rtn = IS_TIME()('21.30')
        self.assertEqual(rtn, (datetime.time(21, 30), None))
        rtn = IS_TIME()('21:30:59')
        self.assertEqual(rtn, (datetime.time(21, 30, 59), None))
        rtn = IS_TIME()('5:30')
        self.assertEqual(rtn, (datetime.time(5, 30), None))
        rtn = IS_TIME()('5:30 am')
        self.assertEqual(rtn, (datetime.time(5, 30), None))
        rtn = IS_TIME()('5:30 pm')
        self.assertEqual(rtn, (datetime.time(17, 30), None))
        rtn = IS_TIME()('5:30 whatever')
        self.assertEqual(rtn, ('5:30 whatever', 'Enter time as hh:mm:ss (seconds, am, pm optional)'))
        rtn = IS_TIME()('5:30 20')
        self.assertEqual(rtn, ('5:30 20', 'Enter time as hh:mm:ss (seconds, am, pm optional)'))
        rtn = IS_TIME()('24:30')
        self.assertEqual(rtn, ('24:30', 'Enter time as hh:mm:ss (seconds, am, pm optional)'))
        rtn = IS_TIME()('21:60')
        self.assertEqual(rtn, ('21:60', 'Enter time as hh:mm:ss (seconds, am, pm optional)'))
        rtn = IS_TIME()('21:30::')
        self.assertEqual(rtn, ('21:30::', 'Enter time as hh:mm:ss (seconds, am, pm optional)'))
        rtn = IS_TIME()('')
        self.assertEqual(rtn, ('', 'Enter time as hh:mm:ss (seconds, am, pm optional)'))

    def test_IS_UPPER(self):
        rtn = IS_UPPER()('abc')
        self.assertEqual(rtn, ('ABC', None))
        rtn = IS_UPPER()('ñ')
        self.assertEqual(rtn, ('\xc3\x91', None))


    def test_IS_JSON(self):
        rtn = IS_JSON()('{"a": 100}')
        self.assertEqual(rtn, ({u'a': 100}, None))
        rtn = IS_JSON()('spam1234')
        self.assertEqual(rtn, ('spam1234', 'Invalid json'))

    def test_IS_UPLOAD_FILENAME(self):
        import cgi
        from StringIO import StringIO
        def gen_fake(filename):
            formdata_file_data = """
---123
Content-Disposition: form-data; name="key2"

value2y
---123
Content-Disposition: form-data; name="file_attach"; filename="%s"
Content-Type: text/plain

this is the content of the fake file

---123--
""" % filename
            formdata_file_environ = {
                'CONTENT_LENGTH':   str(len(formdata_file_data)),
                'CONTENT_TYPE':     'multipart/form-data; boundary=-123',
                'QUERY_STRING':     'key1=value1&key2=value2x',
                'REQUEST_METHOD':   'POST',
            }
            return cgi.FieldStorage(fp=StringIO(formdata_file_data), environ=formdata_file_environ)['file_attach']

        fake = gen_fake('example.pdf')
        rtn = IS_UPLOAD_FILENAME(extension='pdf')(fake)
        self.assertEqual(rtn, (fake, None))
        fake = gen_fake('example.gif')
        rtn = IS_UPLOAD_FILENAME(extension='pdf')(fake)
        self.assertEqual(rtn, (fake, 'Enter valid filename'))
        fake = gen_fake('backup2014.tar.gz')
        rtn = IS_UPLOAD_FILENAME(filename='backup.*', extension='tar.gz', lastdot=False)(fake)
        self.assertEqual(rtn, (fake, None))
        fake = gen_fake('README')
        rtn = IS_UPLOAD_FILENAME(filename='^README$', extension='^$', case=0)(fake)
        self.assertEqual(rtn, (fake, None))
        fake = gen_fake('readme')
        rtn = IS_UPLOAD_FILENAME(filename='^README$', extension='^$', case=0)(fake)
        self.assertEqual(rtn, (fake, 'Enter valid filename'))
        fake = gen_fake('readme')
        rtn = IS_UPLOAD_FILENAME(filename='README', case=2)(fake)
        self.assertEqual(rtn, (fake, None))
        fake = gen_fake('README')
        rtn = IS_UPLOAD_FILENAME(filename='README', case=2)(fake)
        self.assertEqual(rtn, (fake, None))
        rtn = IS_UPLOAD_FILENAME(extension='pdf')('example.pdf')
        self.assertEqual(rtn, ('example.pdf', 'Enter valid filename'))




if __name__ == '__main__':
    unittest.main()
