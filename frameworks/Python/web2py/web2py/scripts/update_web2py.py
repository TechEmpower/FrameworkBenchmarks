#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
crontab -e
* 3 * * * root path/to/this/file
"""

USER = 'www-data'
TMPFILENAME = 'web2py_src_update.zip'

import sys
import os
import urllib
import zipfile

if len(sys.argv) > 1 and sys.argv[1] == 'nightly':
    version = 'http://web2py.com/examples/static/nightly/web2py_src.zip'
else:
    version = 'http://web2py.com/examples/static/web2py_src.zip'

realpath = os.path.realpath(__file__)
path = os.path.dirname(os.path.dirname(os.path.dirname(realpath)))
os.chdir(path)
try:
    old_version = open('web2py/VERSION', 'r').read().strip()
except IOError:
    old_version = ''
open(TMPFILENAME, 'wb').write(urllib.urlopen(version).read())
new_version = zipfile.ZipFile(TMPFILENAME).read('web2py/VERSION').strip()
if new_version > old_version:
    os.system('sudo -u %s unzip -o %s' % (USER, TMPFILENAME))
    os.system('apachectl restart | apache2ctl restart')
