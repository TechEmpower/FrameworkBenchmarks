import sys
import hashlib
import os

PY2 = sys.version_info[0] == 2

if PY2:
    import cPickle as pickle
    import cStringIO as StringIO
    import copy_reg as copyreg
    hashlib_md5 = hashlib.md5
    iterkeys = lambda d: d.iterkeys()
    itervalues = lambda d: d.itervalues()
    iteritems = lambda d: d.iteritems()
else:
    import pickle
    from io import StringIO
    import copyreg
    hashlib_md5 = lambda s: hashlib.md5(bytes(s, 'utf8'))
    iterkeys = lambda d: iter(d.keys())
    itervalues = lambda d: iter(d.values())
    iteritems = lambda d: iter(d.items())

pjoin = os.path.join
exists = os.path.exists
ogetattr = object.__getattribute__
osetattr = object.__setattr__
