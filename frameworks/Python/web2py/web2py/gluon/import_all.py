#!/usr/bin/env python

"""
This file is part of the web2py Web Framework
Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

This file is not strictly required by web2py. It is used for three purposes:

1) check that all required modules are installed properly
2) provide py2exe and py2app a list of modules to be packaged in the binary
3) (optional) preload modules in memory to speed up http responses

"""

import os
import sys

base_modules = ['aifc', 'anydbm', 'array', 'asynchat', 'asyncore', 'atexit',
                'audioop', 'base64', 'BaseHTTPServer', 'Bastion', 'binascii',
                'binhex', 'bisect', 'bz2', 'calendar', 'cgi', 'CGIHTTPServer',
                'cgitb', 'chunk', 'cmath', 'cmd', 'code', 'codecs', 'codeop',
                'collections', 'colorsys', 'compileall', 'compiler',
                'compiler.ast', 'compiler.visitor', 'ConfigParser',
                'contextlib', 'Cookie', 'cookielib', 'copy', 'copy_reg',
                'collections',
                'cPickle', 'cProfile', 'cStringIO', 'csv', 'ctypes',
                'datetime', 'decimal', 'difflib', 'dircache', 'dis',
                'doctest', 'DocXMLRPCServer', 'dumbdbm', 'dummy_thread',
                'dummy_threading', 'email', 'email.charset', 'email.encoders',
                'email.errors', 'email.generator', 'email.header',
                'email.iterators', 'email.message', 'email.mime',
                'email.mime.audio', 'email.mime.base', 'email.mime.image',
                'email.mime.message', 'email.mime.multipart',
                'email.mime.nonmultipart', 'email.mime.text', 'email.parser',
                'email.utils', 'encodings.idna', 'errno', 'exceptions',
                'filecmp', 'fileinput', 'fnmatch', 'formatter', 'fpformat',
                'ftplib', 'functools', 'gc', 'getopt', 'getpass', 'gettext',
                'glob', 'gzip', 'hashlib', 'heapq', 'hmac', 'hotshot',
                'hotshot.stats', 'htmlentitydefs', 'htmllib', 'HTMLParser',
                'httplib', 'imaplib', 'imghdr', 'imp', 'inspect',
                'itertools', 'keyword', 'linecache', 'locale', 'logging',
                'macpath', 'mailbox', 'mailcap', 'marshal', 'math',
                'mimetools', 'mimetypes', 'mmap', 'modulefinder', 'mutex',
                'netrc', 'new', 'nntplib', 'operator', 'optparse', 'os',
                'parser', 'pdb', 'pickle', 'pickletools', 'pkgutil',
                'platform', 'poplib', 'pprint', 'py_compile', 'pyclbr',
                'pydoc', 'Queue', 'quopri', 'random', 're', 'repr',
                'rexec', 'rfc822', 'rlcompleter', 'robotparser', 'runpy',
                'sched', 'select', 'sgmllib', 'shelve',
                'shlex', 'shutil', 'signal', 'SimpleHTTPServer',
                'SimpleXMLRPCServer', 'site', 'smtpd', 'smtplib',
                'sndhdr', 'socket', 'SocketServer', 'sqlite3',
                'stat', 'statvfs', 'string', 'StringIO',
                'stringprep', 'struct', 'subprocess', 'sunau', 'symbol',
                'tabnanny', 'tarfile', 'telnetlib', 'tempfile', 'textwrap', 'thread', 'threading',
                'time', 'timeit', 'Tix', 'Tkinter', 'token',
                'tokenize', 'trace', 'traceback', 'types',
                'unicodedata', 'unittest', 'urllib', 'urllib2',
                'urlparse', 'user', 'UserDict', 'UserList', 'UserString',
                'uu', 'uuid', 'warnings', 'wave', 'weakref', 'webbrowser',
                'whichdb', 'wsgiref', 'wsgiref.handlers', 'wsgiref.headers',
                'wsgiref.simple_server', 'wsgiref.util', 'wsgiref.validate',
                'xdrlib', 'xml.dom', 'xml.dom.minidom', 'xml.dom.pulldom',
                'xml.etree.ElementTree', 'xml.parsers.expat', 'xml.sax',
                'xml.sax.handler', 'xml.sax.saxutils', 'xml.sax.xmlreader',
                'xmlrpclib', 'zipfile', 'zipimport', 'zlib', 'mhlib',
                'MimeWriter', 'mimify', 'multifile', 'sets']

contributed_modules = []

# Python base version
python_version = sys.version[:3]

# Modules which we want to raise an Exception if they are missing
alert_dependency = ['hashlib', 'uuid']

# Now we remove the blacklisted modules if we are using the stated
# python version.
#
# List of modules deprecated in Python 2.6 or 2.7 that are in the above set
py26_deprecated = ['mhlib', 'multifile', 'mimify', 'sets', 'MimeWriter']
py27_deprecated = []  # ['optparse'] but we need it for now

if python_version >= '2.6':
    base_modules += ['json', 'multiprocessing']
    base_modules = list(set(base_modules).difference(set(py26_deprecated)))

if python_version >= '2.7':
    base_modules += ['argparse']
    base_modules = list(set(base_modules).difference(set(py27_deprecated)))

# Now iterate in the base_modules, trying to do the import
for module in base_modules + contributed_modules:
    try:
        __import__(module, globals(), locals(), [])
    except:
        # Raise an exception if the current module is a dependency
        if module in alert_dependency:
            msg = "Missing dependency: %(module)s\n" % locals()
            msg += "Try the following command: "
            msg += "easy_install-%(python_version)s -U %(module)s" % locals()
            raise ImportError(msg)
