import sys

if sys.version < "2.7":
    import unittest2 as unittest
else:
    import unittest
