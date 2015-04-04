#!/bin/env python
# -*- coding: utf-8 -*-
"""
Use to update official_top_level_domains in gluon/validators.py
"""

import itertools
import operator
import urllib2

LIMIT = 70
PREFIX = '    '
TLDS_URL = 'http://data.iana.org/TLD/tlds-alpha-by-domain.txt'

resp = urllib2.urlopen(TLDS_URL)
content = resp.read()

valid_lines = [a.strip().lower() for a in content.split('\n') if a.strip() and a.strip()[0] != '#']
valid_lines += ['localhost']

print 'Fetched TLDs are %s' % len(valid_lines)

results = [list(g) for k, g in itertools.groupby(sorted(valid_lines), key=operator.itemgetter(0))]

output = []
line = "'%s', "

for a in results:
    output.append('%s# %s' % (PREFIX, a[-1][0]))
    thisline = PREFIX
    for c in a:
        newline = thisline + line % c
        if len(newline) > 70:
            output.append(thisline[:-1])
            thisline = PREFIX + line % c
        else:
            thisline += line % c
    if thisline:
        output.append(thisline[:-1])

print '[\n' + '\n'.join(output)[:-1] + '\n]'
