#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
| Created by Vladyslav Kozlovskyy (Ukraine) <dbdevelop©gmail.com>
| for Web2py project

Utilities and class for UTF8 strings managing
----------------------------------------------
"""
import __builtin__
__all__ = ['Utf8']

repr_escape_tab = {}
for i in range(1, 32):
    repr_escape_tab[i] = ur'\x%02x' % i
repr_escape_tab[7] = u'\\a'
repr_escape_tab[8] = u'\\b'
repr_escape_tab[9] = u'\\t'
repr_escape_tab[10] = u'\\n'
repr_escape_tab[11] = u'\\v'
repr_escape_tab[12] = u'\\f'
repr_escape_tab[13] = u'\\r'
repr_escape_tab[ord('\\')] = u'\\\\'
repr_escape_tab2 = repr_escape_tab.copy()
repr_escape_tab2[ord('\'')] = u"\\'"


def sort_key(s):
    """Unicode Collation Algorithm (UCA) (http://www.unicode.org/reports/tr10/)
    is used for utf-8 and unicode strings sorting and for utf-8 strings
    comparison

    Note:
        pyuca is a very memory cost module! It loads the whole
        "allkey.txt" file (~2mb!) into the memory. But this
        functionality is needed only when sort_key() is called as a
        part of sort() function or when Utf8 strings are compared.

    So, it is a lazy "sort_key" function which (ONLY ONCE, ON ITS
    FIRST CALL) imports pyuca and replaces itself with a real
    sort_key() function
    """
    global sort_key
    try:
        from gluon.contrib.pyuca import unicode_collator
        unicode_sort_key = unicode_collator.sort_key
        sort_key = lambda s: unicode_sort_key(
            unicode(s, 'utf-8') if isinstance(s, str) else s)
    except:
        sort_key = lambda s: (
            unicode(s, 'utf-8') if isinstance(s, str) else s).lower()
    return sort_key(s)


def ord(char):
    """Returns unicode id for utf8 or unicode *char* character
    SUPPOSE that *char* is an utf-8 or unicode character only
    """
    if isinstance(char, unicode):
        return __builtin__.ord(char)
    return __builtin__.ord(unicode(char, 'utf-8'))


def chr(code):
    """Returns utf8-character with *code* unicode id """
    return Utf8(unichr(code))


def size(string):
    """Returns length of utf-8 string in bytes

    Note:
        The length of correspondent utf-8 string is returned for unicode string
    """
    return Utf8(string).__size__()


def truncate(string, length, dots='...'):
    """Returns string of length < *length* or truncate string with adding
    *dots* suffix to the string's end

    Args:
        length (int): max length of string
        dots (str or unicode): string suffix, when string is cutted

    Returns:
        (utf8-str): original or cutted string
    """
    text = unicode(string, 'utf-8')
    dots = unicode(dots, 'utf-8') if isinstance(dots, str) else dots
    if len(text) > length:
        text = text[:length - len(dots)] + dots
    return str.__new__(Utf8, text.encode('utf-8'))


class Utf8(str):
    """
    Class for utf8 string storing and manipulations

    The base presupposition of this class usage is:
    "ALL strings in the application are either of
    utf-8 or unicode type, even when simple str
    type is used. UTF-8 is only a "packed" version
    of unicode, so Utf-8 and unicode strings are
    interchangeable."

    CAUTION! This class is slower than str/unicode!
    Do NOT use it inside intensive loops. Simply
    decode string(s) to unicode before loop and
    encode it back to utf-8 string(s) after
    intensive calculation.

    You can see the benefit of this class in doctests() below
    """
    def __new__(cls, content='', codepage='utf-8'):
        if isinstance(content, unicode):
            return str.__new__(cls, unicode.encode(content, 'utf-8'))
        elif codepage in ('utf-8', 'utf8') or isinstance(content, cls):
            return str.__new__(cls, content)
        else:
            return str.__new__(cls, unicode(content, codepage).encode('utf-8'))

    def __repr__(self):
        r''' # note that we use raw strings to avoid having to use double back slashes below
        NOTE! This function is a clone of web2py:gluon.languages.utf_repl() function::

            utf8.__repr__() works same as str.repr() when processing ascii string
            >>> repr(Utf8('abc')) == repr(Utf8("abc")) == repr('abc') == repr("abc") == "'abc'"
            True
            >>> repr(Utf8('a"b"c')) == repr('a"b"c') == '\'a"b"c\''
            True
            >>> repr(Utf8("a'b'c")) == repr("a'b'c") == '"a\'b\'c"'
            True
            >>> repr(Utf8('a\'b"c')) == repr('a\'b"c') == repr(Utf8("a'b\"c")) == repr("a'b\"c") == '\'a\\\'b"c\''
            True
            >>> repr(Utf8('a\r\nb')) == repr('a\r\nb') == "'a\\r\\nb'" # Test for \r, \n
            True

        Unlike str.repr(), Utf8.__repr__() remains utf8 content when processing utf8 string::

            >>> repr(Utf8('中文字')) == repr(Utf8("中文字")) == "'中文字'" != repr('中文字')
            True
            >>> repr(Utf8('中"文"字')) == "'中\"文\"字'" != repr('中"文"字')
            True
            >>> repr(Utf8("中'文'字")) == '"中\'文\'字"' != repr("中'文'字")
            True
            >>> repr(Utf8('中\'文"字')) == repr(Utf8("中'文\"字")) == '\'中\\\'文"字\'' != repr('中\'文"字') == repr("中'文\"字")
            True
            >>> repr(Utf8('中\r\n文')) == "'中\\r\\n文'" != repr('中\r\n文') # Test for \r, \n
            True
        '''
        if str.find(self, "'") >= 0 and str.find(self, '"') < 0:  # only single quote exists
            return '"' + unicode(self, 'utf-8').translate(repr_escape_tab).encode('utf-8') + '"'
        else:
            return "'" + unicode(self, 'utf-8').translate(repr_escape_tab2).encode('utf-8') + "'"

    def __size__(self):
        """ length of utf-8 string in bytes """
        return str.__len__(self)

    def __contains__(self, other):
        return str.__contains__(self, Utf8(other))

    def __getitem__(self, index):
        return str.__new__(Utf8, unicode(self, 'utf-8')[index].encode('utf-8'))

    def __getslice__(self, begin, end):
        return str.__new__(Utf8, unicode(self, 'utf-8')[begin:end].encode('utf-8'))

    def __add__(self, other):
        return str.__new__(Utf8, str.__add__(self, unicode.encode(other, 'utf-8')
                                             if isinstance(other, unicode) else other))

    def __len__(self):
        return len(unicode(self, 'utf-8'))

    def __mul__(self, integer):
        return str.__new__(Utf8, str.__mul__(self, integer))

    def __eq__(self, string):
        return str.__eq__(self, Utf8(string))

    def __ne__(self, string):
        return str.__ne__(self, Utf8(string))

    def capitalize(self):
        return str.__new__(Utf8, unicode(self, 'utf-8').capitalize().encode('utf-8'))

    def center(self, length):
        return str.__new__(Utf8, unicode(self, 'utf-8').center(length).encode('utf-8'))

    def upper(self):
        return str.__new__(Utf8, unicode(self, 'utf-8').upper().encode('utf-8'))

    def lower(self):
        return str.__new__(Utf8, unicode(self, 'utf-8').lower().encode('utf-8'))

    def title(self):
        return str.__new__(Utf8, unicode(self, 'utf-8').title().encode('utf-8'))

    def index(self, string):
        return unicode(self, 'utf-8').index(string if isinstance(string, unicode) else unicode(string, 'utf-8'))

    def isalnum(self):
        return unicode(self, 'utf-8').isalnum()

    def isalpha(self):
        return unicode(self, 'utf-8').isalpha()

    def isdigit(self):
        return unicode(self, 'utf-8').isdigit()

    def islower(self):
        return unicode(self, 'utf-8').islower()

    def isspace(self):
        return unicode(self, 'utf-8').isspace()

    def istitle(self):
        return unicode(self, 'utf-8').istitle()

    def isupper(self):
        return unicode(self, 'utf-8').isupper()

    def zfill(self, length):
        return str.__new__(Utf8, unicode(self, 'utf-8').zfill(length).encode('utf-8'))

    def join(self, iter):
        return str.__new__(Utf8, str.join(self, [Utf8(c) for c in
                                                 list(unicode(iter, 'utf-8') if
                                                      isinstance(iter, str) else
                                                      iter)]))

    def lstrip(self, chars=None):
        return str.__new__(Utf8, str.lstrip(self, None if chars is None else Utf8(chars)))

    def rstrip(self, chars=None):
        return str.__new__(Utf8, str.rstrip(self, None if chars is None else Utf8(chars)))

    def strip(self, chars=None):
        return str.__new__(Utf8, str.strip(self, None if chars is None else Utf8(chars)))

    def swapcase(self):
        return str.__new__(Utf8, unicode(self, 'utf-8').swapcase().encode('utf-8'))

    def count(self, sub, start=0, end=None):
        unistr = unicode(self, 'utf-8')
        return unistr.count(
            unicode(sub, 'utf-8') if isinstance(sub, str) else sub,
            start, len(unistr) if end is None else end)

    def decode(self, encoding='utf-8', errors='strict'):
        return str.decode(self, encoding, errors)

    def encode(self, encoding, errors='strict'):
        return unicode(self, 'utf-8').encode(encoding, errors)

    def expandtabs(self, tabsize=8):
        return str.__new__(Utf8, unicode(self, 'utf-8').expandtabs(tabsize).encode('utf-8'))

    def find(self, sub, start=None, end=None):
        return unicode(self, 'utf-8').find(unicode(sub, 'utf-8')
                                           if isinstance(sub, str) else sub, start, end)

    def ljust(self, width, fillchar=' '):
        return str.__new__(Utf8, unicode(self, 'utf-8').ljust(width, unicode(fillchar, 'utf-8')
                                                              if isinstance(fillchar, str) else fillchar).encode('utf-8'))

    def partition(self, sep):
        (head, sep, tail) = str.partition(self, Utf8(sep))
        return (str.__new__(Utf8, head),
                str.__new__(Utf8, sep),
                str.__new__(Utf8, tail))

    def replace(self, old, new, count=-1):
        return str.__new__(Utf8, str.replace(self, Utf8(old), Utf8(new), count))

    def rfind(self, sub, start=None, end=None):
        return unicode(self, 'utf-8').rfind(unicode(sub, 'utf-8')
                                            if isinstance(sub, str) else sub, start, end)

    def rindex(self, string):
        return unicode(self, 'utf-8').rindex(string if isinstance(string, unicode)
                                             else unicode(string, 'utf-8'))

    def rjust(self, width, fillchar=' '):
        return str.__new__(Utf8, unicode(self, 'utf-8').rjust(width, unicode(fillchar, 'utf-8')
                                                              if isinstance(fillchar, str) else fillchar).encode('utf-8'))

    def rpartition(self, sep):
        (head, sep, tail) = str.rpartition(self, Utf8(sep))
        return (str.__new__(Utf8, head),
                str.__new__(Utf8, sep),
                str.__new__(Utf8, tail))

    def rsplit(self, sep=None, maxsplit=-1):
        return [str.__new__(Utf8, part) for part in str.rsplit(self,
                                                               None if sep is None else Utf8(sep), maxsplit)]

    def split(self, sep=None, maxsplit=-1):
        return [str.__new__(Utf8, part) for part in str.split(self,
                                                              None if sep is None else Utf8(sep), maxsplit)]

    def splitlines(self, keepends=False):
        return [str.__new__(Utf8, part) for part in str.splitlines(self, keepends)]

    def startswith(self, prefix, start=0, end=None):
        unistr = unicode(self, 'utf-8')
        if isinstance(prefix, tuple):
            prefix = tuple(unicode(
                s, 'utf-8') if isinstance(s, str) else s for s in prefix)
        elif isinstance(prefix, str):
            prefix = unicode(prefix, 'utf-8')
        return unistr.startswith(prefix, start, len(unistr) if end is None else end)

    def translate(self, table, deletechars=''):
        if isinstance(table, dict):
            return str.__new__(Utf8, unicode(self, 'utf-8').translate(table).encode('utf-8'))
        else:
            return str.__new__(Utf8, str.translate(self, table, deletechars))

    def endswith(self, prefix, start=0, end=None):
        unistr = unicode(self, 'utf-8')
        if isinstance(prefix, tuple):
            prefix = tuple(unicode(
                s, 'utf-8') if isinstance(s, str) else s for s in prefix)
        elif isinstance(prefix, str):
            prefix = unicode(prefix, 'utf-8')
        return unistr.endswith(prefix, start, len(unistr) if end is None else end)
    if hasattr(str, 'format'):  # Python 2.5 hasn't got str.format() method
        def format(self, *args, **kwargs):
            args = [unicode(
                s, 'utf-8') if isinstance(s, str) else s for s in args]
            kwargs = dict((unicode(k, 'utf-8') if isinstance(k, str) else k,
                           unicode(v, 'utf-8') if isinstance(v, str) else v)
                          for k, v in kwargs.iteritems())
            return str.__new__(Utf8, unicode(self, 'utf-8').
                               format(*args, **kwargs).encode('utf-8'))

    def __mod__(self, right):
        if isinstance(right, tuple):
            right = tuple(unicode(v, 'utf-8') if isinstance(v, str) else v
                          for v in right)
        elif isinstance(right, dict):
            right = dict((unicode(k, 'utf-8') if isinstance(k, str) else k,
                          unicode(v, 'utf-8') if isinstance(v, str) else v)
                         for k, v in right.iteritems())
        elif isinstance(right, str):
            right = unicode(right, 'utf-8')
        return str.__new__(Utf8, unicode(self, 'utf-8').__mod__(right).encode('utf-8'))

    def __ge__(self, string):
        return sort_key(self) >= sort_key(string)

    def __gt__(self, string):
        return sort_key(self) > sort_key(string)

    def __le__(self, string):
        return sort_key(self) <= sort_key(string)

    def __lt__(self, string):
        return sort_key(self) < sort_key(string)


if __name__ == '__main__':
    def doctests():
        u"""
        doctests:
        >>> test_unicode=u'ПРоба Є PRobe'
        >>> test_unicode_word=u'ПРоба'
        >>> test_number_str='12345'
        >>> test_unicode
        u'\\u041f\\u0420\\u043e\\u0431\\u0430 \\u0404 PRobe'
        >>> print test_unicode
        ПРоба Є PRobe
        >>> test_word=test_unicode_word.encode('utf-8')
        >>> test_str=test_unicode.encode('utf-8')
        >>> s=Utf8(test_str)
        >>> s
        'ПРоба Є PRobe'
        >>> type(s)
        <class '__main__.Utf8'>
        >>> s == test_str
        True
        >>> len(test_str) # wrong length of utf8-string!
        19
        >>> len(test_unicode) # RIGHT!
        13
        >>> len(s) # RIGHT!
        13
        >>> size(test_str) # size of utf-8 string (in bytes) == len(str)
        19
        >>> size(test_unicode) # size of unicode string in bytes (packed to utf-8 string)
        19
        >>> size(s) # size of utf-8 string in bytes
        19
        >>> try: # utf-8 is a multibyte string. Convert it to unicode for use with builtin ord()
        ...     __builtin__.ord('б')  #  ascii string
        ... except Exception, e:
        ...     print 'Exception:', e
        Exception: ord() expected a character, but string of length 2 found
        >>> ord('б') # utf8.ord() is used(!!!)
        1073
        >>> ord(u'б') # utf8.ord() is used(!!!)
        1073
        >>> ord(s[3])  # utf8.ord() is used(!!!)
        1073
        >>> chr(ord(s[3])) # utf8.chr() and utf8.chr() is used(!!!)
        'б'
        >>> type(chr(1073))  # utf8.chr() is used(!!!)
        <class '__main__.Utf8'>
        >>> s=Utf8(test_unicode)
        >>> s
        'ПРоба Є PRobe'
        >>> s == test_str
        True
        >>> test_str == s
        True
        >>> s == test_unicode
        True
        >>> test_unicode == s
        True
        >>> print test_str.upper() # only ASCII characters uppered
        ПРоба Є PROBE
        >>> print test_unicode.upper() # unicode gives right result
        ПРОБА Є PROBE
        >>> s.upper() # utf8 class use unicode.upper()
        'ПРОБА Є PROBE'
        >>> type(s.upper())
        <class '__main__.Utf8'>
        >>> s.lower()
        'проба є probe'
        >>> type(s.lower())
        <class '__main__.Utf8'>
        >>> s.capitalize()
        'Проба є probe'
        >>> type(s.capitalize())
        <class '__main__.Utf8'>
        >>> len(s)
        13
        >>> len(test_unicode)
        13
        >>> s+'. Probe is проба'
        'ПРоба Є PRobe. Probe is проба'
        >>> type(s+'. Probe is проба')
        <class '__main__.Utf8'>
        >>> s+u'. Probe is проба'
        'ПРоба Є PRobe. Probe is проба'
        >>> type(s+u'. Probe is проба')
        <class '__main__.Utf8'>
        >>> s+s
        'ПРоба Є PRobeПРоба Є PRobe'
        >>> type(s+s)
        <class '__main__.Utf8'>
        >>> a=s
        >>> a+=s
        >>> a+=test_unicode
        >>> a+=test_str
        >>> a
        'ПРоба Є PRobeПРоба Є PRobeПРоба Є PRobeПРоба Є PRobe'
        >>> type(a)
        <class '__main__.Utf8'>
        >>> s*3
        'ПРоба Є PRobeПРоба Є PRobeПРоба Є PRobe'
        >>> type(s*3)
        <class '__main__.Utf8'>
        >>> a=Utf8("-проба-")
        >>> a*=10
        >>> a
        '-проба--проба--проба--проба--проба--проба--проба--проба--проба--проба-'
        >>> type(a)
        <class '__main__.Utf8'>
        >>> print "'"+test_str.center(17)+"'" # WRONG RESULT!
        'ПРоба Є PRobe'
        >>> s.center(17) # RIGHT!
        '  ПРоба Є PRobe  '
        >>> type(s.center(17))
        <class '__main__.Utf8'>
        >>> (test_word+test_number_str).isalnum() # WRONG RESULT! non ASCII chars are detected as non alpha
        False
        >>> Utf8(test_word+test_number_str).isalnum()
        True
        >>> s.isalnum()
        False
        >>> test_word.isalpha() # WRONG RESULT! Non ASCII characters are detected as non alpha
        False
        >>> Utf8(test_word).isalpha() # RIGHT!
        True
        >>> s.lower().islower()
        True
        >>> s.upper().isupper()
        True
        >>> print test_str.zfill(17) # WRONG RESULT!
        ПРоба Є PRobe
        >>> s.zfill(17) # RIGHT!
        '0000ПРоба Є PRobe'
        >>> type(s.zfill(17))
        <class '__main__.Utf8'>
        >>> s.istitle()
        False
        >>> s.title().istitle()
        True
        >>> Utf8('1234').isdigit()
        True
        >>> Utf8(' \t').isspace()
        True
        >>> s.join('•|•')
        '•ПРоба Є PRobe|ПРоба Є PRobe•'
        >>> s.join((str('(utf8 тест1)'), unicode('(unicode тест2)','utf-8'), '(ascii test3)'))
        '(utf8 тест1)ПРоба Є PRobe(unicode тест2)ПРоба Є PRobe(ascii test3)'
        >>> type(s)
        <class '__main__.Utf8'>
        >>> s==test_str
        True
        >>> s==test_unicode
        True
        >>> s.swapcase()
        'прОБА є prOBE'
        >>> type(s.swapcase())
        <class '__main__.Utf8'>
        >>> truncate(s, 10)
        'ПРоба Є...'
        >>> truncate(s, 20)
        'ПРоба Є PRobe'
        >>> truncate(s, 10, '•••') # utf-8 string as *dots*
        'ПРоба Є•••'
        >>> truncate(s, 10, u'®') # you can use unicode string as *dots*
        'ПРоба Є P®'
        >>> type(truncate(s, 10))
        <class '__main__.Utf8'>
        >>> Utf8(s.encode('koi8-u'), 'koi8-u')
        'ПРоба Є PRobe'
        >>> s.decode() # convert utf-8 string to unicode
        u'\\u041f\\u0420\\u043e\\u0431\\u0430 \\u0404 PRobe'
        >>> a='про\\tba'
        >>> str_tmp=a.expandtabs()
        >>> utf8_tmp=Utf8(a).expandtabs()
        >>> utf8_tmp.replace(' ','.') # RIGHT! (default tabsize is 8)
        'про.....ba'
        >>> utf8_tmp.index('b')
        8
        >>> print "'"+str_tmp.replace(' ','.')+"'" # WRONG STRING LENGTH!
        'про..ba'
        >>> str_tmp.index('b') # WRONG index of 'b' character
        8
        >>> print "'"+a.expandtabs(4).replace(' ','.')+"'" # WRONG RESULT!
        'про..ba'
        >>> Utf8(a).expandtabs(4).replace(' ','.') # RIGHT!
        'про.ba'
        >>> s.find('Є')
        6
        >>> s.find(u'Є')
        6
        >>> s.find(' ', 6)
        7
        >>> s.rfind(' ')
        7
        >>> s.partition('Є')
        ('ПРоба ', 'Є', ' PRobe')
        >>> s.partition(u'Є')
        ('ПРоба ', 'Є', ' PRobe')
        >>> (a,b,c) = s.partition('Є')
        >>> type(a), type(b), type(c)
        (<class '__main__.Utf8'>, <class '__main__.Utf8'>, <class '__main__.Utf8'>)
        >>> s.partition(' ')
        ('ПРоба', ' ', 'Є PRobe')
        >>> s.rpartition(' ')
        ('ПРоба Є', ' ', 'PRobe')
        >>> s.index('Є')
        6
        >>> s.rindex(u'Є')
        6
        >>> s.index(' ')
        5
        >>> s.rindex(' ')
        7
        >>> a=Utf8('а б ц д е а б ц д е а\\tб ц д е')
        >>> a.split()
        ['а', 'б', 'ц', 'д', 'е', 'а', 'б', 'ц', 'д', 'е', 'а', 'б', 'ц', 'д', 'е']
        >>> a.rsplit()
        ['а', 'б', 'ц', 'д', 'е', 'а', 'б', 'ц', 'д', 'е', 'а', 'б', 'ц', 'д', 'е']
        >>> a.expandtabs().split('б')
        ['а ', ' ц д е а ', ' ц д е а   ', ' ц д е']
        >>> a.expandtabs().rsplit('б')
        ['а ', ' ц д е а ', ' ц д е а   ', ' ц д е']
        >>> a.expandtabs().split(u'б', 1)
        ['а ', ' ц д е а б ц д е а   б ц д е']
        >>> a.expandtabs().rsplit(u'б', 1)
        ['а б ц д е а б ц д е а   ', ' ц д е']
        >>> a=Utf8("рядок1\\nрядок2\\nрядок3")
        >>> a.splitlines()
        ['рядок1', 'рядок2', 'рядок3']
        >>> a.splitlines(True)
        ['рядок1\\n', 'рядок2\\n', 'рядок3']
        >>> s[6]
        'Є'
        >>> s[0]
        'П'
        >>> s[-1]
        'e'
        >>> s[:10]
        'ПРоба Є PR'
        >>> s[2:-2:2]
        'оаЄPo'
        >>> s[::-1]
        'eboRP Є абоРП'
        >>> s.startswith('ПР')
        True
        >>> s.startswith(('ПР', u'об'),0)
        True
        >>> s.startswith(u'об', 2, 4)
        True
        >>> s.endswith('be')
        True
        >>> s.endswith(('be', 'PR', u'Є'))
        True
        >>> s.endswith('PR', 8, 10)
        True
        >>> s.endswith('Є', -7, -6)
        True
        >>> s.count(' ')
        2
        >>> s.count(' ',6)
        1
        >>> s.count(u'Є')
        1
        >>> s.count('Є', 0, 5)
        0
        >>> Utf8("Parameters: '%(проба)s', %(probe)04d, %(проба2)s") % { u"проба": s,
        ...      "not used": "???", "probe":  2, "проба2": u"ПРоба Probe" }
        "Parameters: 'ПРоба Є PRobe', 0002, ПРоба Probe"
        >>> a=Utf8(u"Параметр: (%s)-(%s)-[%s]")
        >>> a%=(s, s[::-1], 1000)
        >>> a
        'Параметр: (ПРоба Є PRobe)-(eboRP Є абоРП)-[1000]'
        >>> if hasattr(Utf8,  'format'):
        ...     Utf8("Проба <{0}>, {1}, {param1}, {param2}").format(s, u"中文字",
        ...           param1="барабан", param2=1000) == 'Проба <ПРоба Є PRobe>, 中文字, барабан, 1000'
        ... else: # format() method is not used in python with version <2.6:
        ...     print True
        True
        >>> u'Б'<u'Ї' # WRONG ORDER!
        False
        >>> 'Б'<'Ї' # WRONG ORDER!
        False
        >>> Utf8('Б')<'Ї' # RIGHT!
        True
        >>> u'д'>u'ґ' # WRONG ORDER!
        False
        >>> Utf8('д')>Utf8('ґ') # RIGHT!
        True
        >>> u'є'<=u'ж' # WRONG ORDER!
        False
        >>> Utf8('є')<=u'ж' # RIGHT!
        True
        >>> Utf8('є')<=u'є'
        True
        >>> u'Ї'>=u'И' # WRONG ORDER!
        False
        >>> Utf8(u'Ї') >= u'И' # RIGHT
        True
        >>> Utf8('Є') >= 'Є'
        True
        >>> a="яжертиуіопшщїасдфгґхйклчєзьцвбнмюЯЖЕРТИУІОПШЩЇАСДФГҐХЙКЛЧЗЬЦВБНМЮЄ"  # str type
        >>> b=u"яжертиуіопшщїасдфгґхйклчєзьцвбнмюЯЖЕРТИУІОПШЩЇАСДФГҐХЙКЛЧЗЬЦВБНМЮЄ" # unicode type
        >>> c=Utf8("яжертиуіопшщїасдфгґхйклчєзьцвбнмюЯЖЕРТИУІОПШЩЇАСДФГҐХЙКЛЧЗЬЦВБНМЮЄ") # utf8 class
        >>> result = "".join(sorted(a))
        >>> result[0:20] # result is not utf8 string, because bytes, not utf8-characters were sorted
        '\\x80\\x81\\x82\\x83\\x84\\x84\\x85\\x86\\x86\\x87\\x87\\x88\\x89\\x8c\\x8e\\x8f\\x90\\x90\\x91\\x91'
        >>> try:
        ...   unicode(result, 'utf-8') # try to convert result (utf-8?) to unicode
        ... except Exception, e:
        ...    print 'Exception:', e
        Exception: 'utf8' codec can't decode byte 0x80 in position 0: unexpected code byte
        >>> try: # FAILED! (working with bytes, not with utf8-charactes)
        ...    "".join( sorted(a, key=sort_key) ) # utf8.sort_key may be used with utf8 or unicode strings only!
        ... except Exception, e:
        ...    print 'Exception:', e
        Exception: 'utf8' codec can't decode byte 0xd1 in position 0: unexpected end of data
        >>> print "".join( sorted(Utf8(a))) # converting *a* to unicode or utf8-string gives us correct result
        аАбБвВгГґҐдДеЕєЄжЖзЗиИіІїЇйЙкКлЛмМнНоОпПрРсСтТуУфФхХцЦчЧшШщЩьЬюЮяЯ
        >>> print u"".join( sorted(b) ) # WRONG ORDER! Default sort key is used
        ЄІЇАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЮЯабвгдежзийклмнопрстуфхцчшщьюяєіїҐґ
        >>> print u"".join( sorted(b, key=sort_key) ) # RIGHT ORDER! utf8.sort_key is used
        аАбБвВгГґҐдДеЕєЄжЖзЗиИіІїЇйЙкКлЛмМнНоОпПрРсСтТуУфФхХцЦчЧшШщЩьЬюЮяЯ
        >>> print "".join( sorted(c) ) # RIGHT ORDER! Utf8 "rich comparison" methods are used
        аАбБвВгГґҐдДеЕєЄжЖзЗиИіІїЇйЙкКлЛмМнНоОпПрРсСтТуУфФхХцЦчЧшШщЩьЬюЮяЯ
        >>> print "".join( sorted(c, key=sort_key) ) # RIGHT ORDER! utf8.sort_key is used
        аАбБвВгГґҐдДеЕєЄжЖзЗиИіІїЇйЙкКлЛмМнНоОпПрРсСтТуУфФхХцЦчЧшШщЩьЬюЮяЯ
        >>> Utf8().join(sorted(c.decode(), key=sort_key)) # convert to unicode for better performance
        'аАбБвВгГґҐдДеЕєЄжЖзЗиИіІїЇйЙкКлЛмМнНоОпПрРсСтТуУфФхХцЦчЧшШщЩьЬюЮяЯ'
        >>> for result in sorted(["Іа", "Астро", u"гала", Utf8("Гоша"), "Єва", "шовк", "аякс", "Їжа",
        ...                       "ґанок", Utf8("Дар'я"), "білінг", "веб", u"Жужа", "проба", u"тест",
        ...                       "абетка", "яблуко", "Юляся", "Київ", "лимонад", "ложка", "Матриця",
        ...                      ], key=sort_key):
        ...     print result.ljust(20), type(result)
        абетка         <type 'str'>
        Астро           <type 'str'>
        аякс             <type 'str'>
        білінг         <type 'str'>
        веб               <type 'str'>
        гала                 <type 'unicode'>
        ґанок           <type 'str'>
        Гоша                 <class '__main__.Utf8'>
        Дар'я                <class '__main__.Utf8'>
        Єва               <type 'str'>
        Жужа                 <type 'unicode'>
        Іа                 <type 'str'>
        Їжа               <type 'str'>
        Київ             <type 'str'>
        лимонад       <type 'str'>
        ложка           <type 'str'>
        Матриця       <type 'str'>
        проба           <type 'str'>
        тест                 <type 'unicode'>
        шовк             <type 'str'>
        Юляся           <type 'str'>
        яблуко         <type 'str'>

        >>> a=Utf8("中文字")
        >>> L=list(a)
        >>> L
        ['中', '文', '字']
        >>> a="".join(L)
        >>> print a
        中文字
        >>> type(a)
        <type 'str'>
        >>> a="中文字"  # standard str type
        >>> L=list(a)
        >>> L
        ['\\xe4', '\\xb8', '\\xad', '\\xe6', '\\x96', '\\x87', '\\xe5', '\\xad', '\\x97']
        >>> from string import maketrans
        >>> str_tab=maketrans('PRobe','12345')
        >>> unicode_tab={ord(u'П'):ord(u'Ж'),
        ...              ord(u'Р')      : u'Ш',
        ...              ord(Utf8('о')) : None,  # utf8.ord() is used
        ...              ord('б')       : None,  # -//-//-
        ...              ord(u'а')      : u"中文字",
        ...              ord(u'Є')      : Utf8('•').decode(), # only unicode type is supported
        ...             }
        >>> s.translate(unicode_tab).translate(str_tab, deletechars=' ')
        'ЖШ中文字•12345'
        """
        import sys
        reload(sys)
        sys.setdefaultencoding("UTF-8")
        import doctest
        print "DOCTESTS STARTED..."
        doctest.testmod()
        print "DOCTESTS FINISHED"

    doctests()
