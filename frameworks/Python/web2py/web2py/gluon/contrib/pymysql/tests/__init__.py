from pymysql.tests.test_issues import *
from pymysql.tests.test_example import *
from pymysql.tests.test_basic import *
from pymysql.tests.test_DictCursor import *

import sys
if sys.version_info[0] == 2:
    # MySQLdb tests were designed for Python 3
    from pymysql.tests.thirdparty import *

if __name__ == "__main__":
    import unittest
    unittest.main()
