#!/usr/bin/env python
# -*- coding: utf-8 -*-

import cStringIO
import re
import sys
import tarfile
import urllib
import xml.parsers.expat as expat

"""
Update script for contenttype.py module.

Usage: python contentupdate.py /path/to/contenttype.py

If no path is specified, script will look for contenttype.py in current
working directory.

Internet connection is required to perform the update.
"""

OVERRIDE = [
    ('.pdb', 'chemical/x-pdb'),
    ('.xyz', 'chemical/x-pdb')
]


class MIMEParser(dict):

    def __start_element_handler(self, name, attrs):
        if name == 'mime-type':
            if self.type:
                for extension in self.extensions:
                    self[extension] = self.type
            self.type = attrs['type'].lower()
            self.extensions = []
        elif name == 'glob':
            pattern = attrs['pattern']
            if pattern.startswith('*.'):
                self.extensions.append(pattern[1:].lower())

    def __init__(self, fileobj):
        dict.__init__(self)
        self.type = ''
        self.extensions = ''
        parser = expat.ParserCreate()
        parser.StartElementHandler = self.__start_element_handler
        parser.ParseFile(fileobj)
        for extension, contenttype in OVERRIDE:
            self[extension] = contenttype


if __name__ == '__main__':
    try:
        path = sys.argv[1]
    except:
        path = 'contenttype.py'
    vregex = re.compile('database version (?P<version>.+?)\.?\n')
    sys.stdout.write('Checking contenttype.py database version:')
    sys.stdout.flush()
    try:
        pathfile = open(path)
        try:
            current = pathfile.read()
        finally:
            pathfile.close()
        cversion = re.search(vregex, current).group('version')
        sys.stdout.write('\t[OK] version %s\n' % cversion)
    except Exception, e:
        sys.stdout.write('\t[ERROR] %s\n' % e)
        exit()
    sys.stdout.write('Checking freedesktop.org database version:')
    sys.stdout.flush()
    try:
        search = re.search(
            '(?P<url>http://freedesktop.org/.+?/shared-mime-info-(?P<version>.+?)\.tar\.(?P<type>[gb]z2?))',
            urllib.urlopen('http://www.freedesktop.org/wiki/Software/shared-mime-info').read())
        url = search.group('url')
        assert url is not None
        nversion = search.group('version')
        assert nversion is not None
        ftype = search.group('type')
        assert ftype is not None
        sys.stdout.write('\t[OK] version %s\n' % nversion)
    except:
        sys.stdout.write('\t[ERROR] unknown version\n')
        exit()
    if cversion == nversion:
        sys.stdout.write('\nContenttype.py database is up to date\n')
        exit()
    try:
        raw_input('\nContenttype.py database updates are available from:\n%s (approx. 0.5MB)\nPress enter to continue or CTRL-C to quit now\nWARNING: this will replace contenttype.py file content IN PLACE' % url)
    except:
        exit()
    sys.stdout.write('\nDownloading new database:')
    sys.stdout.flush()
    fregex = re.compile('^.*/freedesktop\.org\.xml$')
    try:
        io = cStringIO.StringIO()
        io.write(urllib.urlopen(url).read())
        sys.stdout.write('\t[OK] done\n')
    except Exception, e:
        sys.stdout.write('\t[ERROR] %s\n' % e)
        exit()
    sys.stdout.write('Installing new database:')
    sys.stdout.flush()
    try:
        tar = tarfile.TarFile.open(fileobj=io, mode='r:%s' % ftype)
        try:
            for content in tar.getnames():
                if fregex.match(content):
                    xml = tar.extractfile(content)
                    break
        finally:
            tar.close()
        data = MIMEParser(xml)
        io = cStringIO.StringIO()
        io.write('CONTENT_TYPE = {\n')
        for key in sorted(data):
            io.write('    \'%s\': \'%s\',\n' % (key, data[key]))
        io.write('    }')
        io.seek(0)
        contenttype = open('contenttype.py', 'w')
        try:
            contenttype.write(re.sub(vregex, 'database version %s.\n' % nversion, re.sub('CONTENT_TYPE = \{(.|\n)+?\}', io.getvalue(), current)))
        finally:
            contenttype.close()
        if not current.closed:
            current.close()
        sys.stdout.write('\t\t\t[OK] done\n')
    except Exception, e:
        sys.stdout.write('\t\t\t[ERROR] %s\n' % e)
