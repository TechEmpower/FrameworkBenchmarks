from _adapt import NOSQL

if NOSQL:
    from nosql import *
else:
    from sql import *

from validation import *
from caching import TestCache
