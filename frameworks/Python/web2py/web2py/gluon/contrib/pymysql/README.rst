====================
PyMySQL Installation
====================

.. contents::
..
   This package contains a pure-Python MySQL client library.
   Documentation on the MySQL client/server protocol can be found here:
   http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol
   If you would like to run the test suite, edit the config parameters in
   pymysql/tests/base.py. The goal of pymysql is to be a drop-in
   replacement for MySQLdb and work on CPython 2.3+, Jython, IronPython, PyPy
   and Python 3. We test for compatibility by simply changing the import
   statements in the Django MySQL backend and running its unit tests as well
   as running it against the MySQLdb and myconnpy unit tests.

Requirements
-------------

+ Python 2.4 or higher

 * http://www.python.org/
 
 * 2.6 is the primary test environment.

* MySQL 4.1 or higher
    
 * protocol41 support, experimental 4.0 support

Installation
------------

# easy_install pymysql
# ... or ...
# python setup.py install

Python 3.0 Support
------------------

Simply run the build-py3k.sh script from the local directory. It will
build a working package in the ./py3k directory.
