#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import re

filename = sys.argv[1]

datafile = open(filename, 'r')
try:
    data = '\n' + datafile.read()
finally:
    datafile.close()
SPACE = '\n    ' if '-n' in sys.argv[1:] else ' '

data = re.compile('(?<!\:)//(?P<a>.*)').sub('/* \g<a> */', data)
data = re.compile('[ ]+').sub(' ', data)
data = re.compile('\s*{\s*').sub(' {' + SPACE, data)
data = re.compile('\s*;\s*').sub(';' + SPACE, data)
data = re.compile(',\s*').sub(', ', data)
data = re.compile('\s*\*/\s*').sub('*/' + SPACE, data)
data = re.compile('\s*}\s*').sub(SPACE + '}\n', data)
data = re.compile('\n\s*\n').sub('\n', data)
data = re.compile(';\s+/\*').sub('; /*', data)
data = re.compile('\*/\s+/\*').sub(' ', data)
data = re.compile('[ ]+\n').sub('\n', data)
data = re.compile('\n\s*/[\*]+(?P<a>.*?)[\*]+/', re.DOTALL).sub(
    '\n/*\g<a>*/\n', data)
data = re.compile('[ \t]+(?P<a>\S.+?){').sub(' \g<a>{', data)
data = data.replace('}', '}\n')

print data
