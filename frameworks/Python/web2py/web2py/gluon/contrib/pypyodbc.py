# -*- coding: utf-8 -*-

# PyPyODBC is develped from RealPyODBC 0.1 beta released in 2004 by Michele Petrazzo. Thanks Michele.

# The MIT License (MIT)
#
# Copyright (c) 2014 Henry Zhou <jiangwen365@gmail.com> and PyPyODBC contributors
# Copyright (c) 2004 Michele Petrazzo

# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
# documentation files (the "Software"), to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial portions
# of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
# THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

pooling = True
apilevel = '2.0'
paramstyle = 'qmark'
threadsafety = 1
version = '1.3.0'
lowercase=True

DEBUG = 0
# Comment out all "if DEBUG:" statements like below for production
#if DEBUG:print 'DEBUGGING'

import sys, os, datetime, ctypes, threading
from decimal import Decimal


py_ver = sys.version[:3]
py_v3 = py_ver >= '3.0'

if py_v3:
    long = int
    unicode = str
    str_8b = bytes
    buffer = memoryview
    BYTE_1 = bytes('1','ascii')
    use_unicode = True
else:
    str_8b = str
    BYTE_1 = '1'
    use_unicode = False
if py_ver < '2.6':
    bytearray = str


if not hasattr(ctypes, 'c_ssize_t'):
    if ctypes.sizeof(ctypes.c_uint) == ctypes.sizeof(ctypes.c_void_p):
        ctypes.c_ssize_t = ctypes.c_int
    elif ctypes.sizeof(ctypes.c_ulong) == ctypes.sizeof(ctypes.c_void_p):
        ctypes.c_ssize_t = ctypes.c_long
    elif ctypes.sizeof(ctypes.c_ulonglong) == ctypes.sizeof(ctypes.c_void_p):
        ctypes.c_ssize_t = ctypes.c_longlong


lock = threading.Lock()
shared_env_h = None
SQLWCHAR_SIZE = ctypes.sizeof(ctypes.c_wchar)

#determin the size of Py_UNICODE
#sys.maxunicode > 65536 and 'UCS4' or 'UCS2'
UNICODE_SIZE = sys.maxunicode > 65536 and 4 or 2


# Define ODBC constants. They are widly used in ODBC documents and programs
# They are defined in cpp header files: sql.h sqlext.h sqltypes.h sqlucode.h
# and you can get these files from the mingw32-runtime_3.13-1_all.deb package
SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC2, SQL_OV_ODBC3 = 200, 2, 3
SQL_DRIVER_NOPROMPT = 0
SQL_ATTR_CONNECTION_POOLING = 201; SQL_CP_ONE_PER_HENV = 2

SQL_FETCH_NEXT, SQL_FETCH_FIRST, SQL_FETCH_LAST = 0x01, 0x02, 0x04
SQL_NULL_HANDLE, SQL_HANDLE_ENV, SQL_HANDLE_DBC, SQL_HANDLE_STMT = 0, 1, 2, 3
SQL_SUCCESS, SQL_SUCCESS_WITH_INFO, SQL_ERROR = 0, 1, -1
SQL_NO_DATA = 100; SQL_NO_TOTAL = -4
SQL_ATTR_ACCESS_MODE = SQL_ACCESS_MODE = 101
SQL_ATTR_AUTOCOMMIT = SQL_AUTOCOMMIT = 102

SQL_MODE_DEFAULT = SQL_MODE_READ_WRITE = 0; SQL_MODE_READ_ONLY = 1
SQL_AUTOCOMMIT_OFF, SQL_AUTOCOMMIT_ON = 0, 1
SQL_IS_UINTEGER = -5
SQL_ATTR_LOGIN_TIMEOUT = 103; SQL_ATTR_CONNECTION_TIMEOUT = 113
SQL_COMMIT, SQL_ROLLBACK = 0, 1

SQL_INDEX_UNIQUE,SQL_INDEX_ALL = 0,1
SQL_QUICK,SQL_ENSURE = 0,1
SQL_FETCH_NEXT = 1
SQL_COLUMN_DISPLAY_SIZE = 6
SQL_INVALID_HANDLE = -2
SQL_NO_DATA_FOUND = 100; SQL_NULL_DATA = -1; SQL_NTS = -3
SQL_HANDLE_DESCR = 4
SQL_TABLE_NAMES = 3
SQL_PARAM_INPUT = 1; SQL_PARAM_INPUT_OUTPUT = 2
SQL_PARAM_TYPE_UNKNOWN = 0
SQL_RESULT_COL = 3
SQL_PARAM_OUTPUT = 4
SQL_RETURN_VALUE = 5
SQL_PARAM_TYPE_DEFAULT = SQL_PARAM_INPUT_OUTPUT

SQL_RESET_PARAMS = 3
SQL_UNBIND = 2
SQL_CLOSE = 0




# Below defines The constants for sqlgetinfo method, and their coresponding return types
SQL_QUALIFIER_LOCATION = 114
SQL_QUALIFIER_NAME_SEPARATOR = 41
SQL_QUALIFIER_TERM = 42
SQL_QUALIFIER_USAGE = 92
SQL_OWNER_TERM = 39
SQL_OWNER_USAGE = 91
SQL_ACCESSIBLE_PROCEDURES = 20
SQL_ACCESSIBLE_TABLES = 19
SQL_ACTIVE_ENVIRONMENTS = 116
SQL_AGGREGATE_FUNCTIONS = 169
SQL_ALTER_DOMAIN = 117
SQL_ALTER_TABLE = 86
SQL_ASYNC_MODE = 10021
SQL_BATCH_ROW_COUNT = 120
SQL_BATCH_SUPPORT = 121
SQL_BOOKMARK_PERSISTENCE = 82
SQL_CATALOG_LOCATION = SQL_QUALIFIER_LOCATION
SQL_CATALOG_NAME = 10003
SQL_CATALOG_NAME_SEPARATOR = SQL_QUALIFIER_NAME_SEPARATOR
SQL_CATALOG_TERM = SQL_QUALIFIER_TERM
SQL_CATALOG_USAGE = SQL_QUALIFIER_USAGE
SQL_COLLATION_SEQ = 10004
SQL_COLUMN_ALIAS = 87
SQL_CONCAT_NULL_BEHAVIOR = 22
SQL_CONVERT_FUNCTIONS = 48
SQL_CONVERT_VARCHAR = 70
SQL_CORRELATION_NAME = 74
SQL_CREATE_ASSERTION = 127
SQL_CREATE_CHARACTER_SET = 128
SQL_CREATE_COLLATION = 129
SQL_CREATE_DOMAIN = 130
SQL_CREATE_SCHEMA = 131
SQL_CREATE_TABLE = 132
SQL_CREATE_TRANSLATION = 133
SQL_CREATE_VIEW = 134
SQL_CURSOR_COMMIT_BEHAVIOR = 23
SQL_CURSOR_ROLLBACK_BEHAVIOR = 24
SQL_DATABASE_NAME = 16
SQL_DATA_SOURCE_NAME = 2
SQL_DATA_SOURCE_READ_ONLY = 25
SQL_DATETIME_LITERALS = 119
SQL_DBMS_NAME = 17
SQL_DBMS_VER = 18
SQL_DDL_INDEX = 170
SQL_DEFAULT_TXN_ISOLATION = 26
SQL_DESCRIBE_PARAMETER = 10002
SQL_DM_VER = 171
SQL_DRIVER_NAME = 6
SQL_DRIVER_ODBC_VER = 77
SQL_DRIVER_VER = 7
SQL_DROP_ASSERTION = 136
SQL_DROP_CHARACTER_SET = 137
SQL_DROP_COLLATION = 138
SQL_DROP_DOMAIN = 139
SQL_DROP_SCHEMA = 140
SQL_DROP_TABLE = 141
SQL_DROP_TRANSLATION = 142
SQL_DROP_VIEW = 143
SQL_DYNAMIC_CURSOR_ATTRIBUTES1 = 144
SQL_DYNAMIC_CURSOR_ATTRIBUTES2 = 145
SQL_EXPRESSIONS_IN_ORDERBY = 27
SQL_FILE_USAGE = 84
SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 = 146
SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 = 147
SQL_GETDATA_EXTENSIONS = 81
SQL_GROUP_BY = 88
SQL_IDENTIFIER_CASE = 28
SQL_IDENTIFIER_QUOTE_CHAR = 29
SQL_INDEX_KEYWORDS = 148
SQL_INFO_SCHEMA_VIEWS = 149
SQL_INSERT_STATEMENT = 172
SQL_INTEGRITY = 73
SQL_KEYSET_CURSOR_ATTRIBUTES1 = 150
SQL_KEYSET_CURSOR_ATTRIBUTES2 = 151
SQL_KEYWORDS = 89
SQL_LIKE_ESCAPE_CLAUSE = 113
SQL_MAX_ASYNC_CONCURRENT_STATEMENTS = 10022
SQL_MAX_BINARY_LITERAL_LEN = 112
SQL_MAX_CATALOG_NAME_LEN = 34
SQL_MAX_CHAR_LITERAL_LEN = 108
SQL_MAX_COLUMNS_IN_GROUP_BY = 97
SQL_MAX_COLUMNS_IN_INDEX = 98
SQL_MAX_COLUMNS_IN_ORDER_BY = 99
SQL_MAX_COLUMNS_IN_SELECT = 100
SQL_MAX_COLUMNS_IN_TABLE = 101
SQL_MAX_COLUMN_NAME_LEN = 30
SQL_MAX_CONCURRENT_ACTIVITIES = 1
SQL_MAX_CURSOR_NAME_LEN = 31
SQL_MAX_DRIVER_CONNECTIONS = 0
SQL_MAX_IDENTIFIER_LEN = 10005
SQL_MAX_INDEX_SIZE = 102
SQL_MAX_PROCEDURE_NAME_LEN = 33
SQL_MAX_ROW_SIZE = 104
SQL_MAX_ROW_SIZE_INCLUDES_LONG = 103
SQL_MAX_SCHEMA_NAME_LEN = 32
SQL_MAX_STATEMENT_LEN = 105
SQL_MAX_TABLES_IN_SELECT = 106
SQL_MAX_TABLE_NAME_LEN = 35
SQL_MAX_USER_NAME_LEN = 107
SQL_MULTIPLE_ACTIVE_TXN = 37
SQL_MULT_RESULT_SETS = 36
SQL_NEED_LONG_DATA_LEN = 111
SQL_NON_NULLABLE_COLUMNS = 75
SQL_NULL_COLLATION = 85
SQL_NUMERIC_FUNCTIONS = 49
SQL_ODBC_INTERFACE_CONFORMANCE = 152
SQL_ODBC_VER = 10
SQL_OJ_CAPABILITIES = 65003
SQL_ORDER_BY_COLUMNS_IN_SELECT = 90
SQL_PARAM_ARRAY_ROW_COUNTS = 153
SQL_PARAM_ARRAY_SELECTS = 154
SQL_PROCEDURES = 21
SQL_PROCEDURE_TERM = 40
SQL_QUOTED_IDENTIFIER_CASE = 93
SQL_ROW_UPDATES = 11
SQL_SCHEMA_TERM = SQL_OWNER_TERM
SQL_SCHEMA_USAGE = SQL_OWNER_USAGE
SQL_SCROLL_OPTIONS = 44
SQL_SEARCH_PATTERN_ESCAPE = 14
SQL_SERVER_NAME = 13
SQL_SPECIAL_CHARACTERS = 94
SQL_SQL92_DATETIME_FUNCTIONS = 155
SQL_SQL92_FOREIGN_KEY_DELETE_RULE = 156
SQL_SQL92_FOREIGN_KEY_UPDATE_RULE = 157
SQL_SQL92_GRANT = 158
SQL_SQL92_NUMERIC_VALUE_FUNCTIONS = 159
SQL_SQL92_PREDICATES = 160
SQL_SQL92_RELATIONAL_JOIN_OPERATORS = 161
SQL_SQL92_REVOKE = 162
SQL_SQL92_ROW_VALUE_CONSTRUCTOR = 163
SQL_SQL92_STRING_FUNCTIONS = 164
SQL_SQL92_VALUE_EXPRESSIONS = 165
SQL_SQL_CONFORMANCE = 118
SQL_STANDARD_CLI_CONFORMANCE = 166
SQL_STATIC_CURSOR_ATTRIBUTES1 = 167
SQL_STATIC_CURSOR_ATTRIBUTES2 = 168
SQL_STRING_FUNCTIONS = 50
SQL_SUBQUERIES = 95
SQL_SYSTEM_FUNCTIONS = 51
SQL_TABLE_TERM = 45
SQL_TIMEDATE_ADD_INTERVALS = 109
SQL_TIMEDATE_DIFF_INTERVALS = 110
SQL_TIMEDATE_FUNCTIONS = 52
SQL_TXN_CAPABLE = 46
SQL_TXN_ISOLATION_OPTION = 72
SQL_UNION = 96
SQL_USER_NAME = 47
SQL_XOPEN_CLI_YEAR = 10000


aInfoTypes = {
SQL_ACCESSIBLE_PROCEDURES : 'GI_YESNO',SQL_ACCESSIBLE_TABLES : 'GI_YESNO',SQL_ACTIVE_ENVIRONMENTS : 'GI_USMALLINT',
SQL_AGGREGATE_FUNCTIONS : 'GI_UINTEGER',SQL_ALTER_DOMAIN : 'GI_UINTEGER',
SQL_ALTER_TABLE : 'GI_UINTEGER',SQL_ASYNC_MODE : 'GI_UINTEGER',SQL_BATCH_ROW_COUNT : 'GI_UINTEGER',
SQL_BATCH_SUPPORT : 'GI_UINTEGER',SQL_BOOKMARK_PERSISTENCE : 'GI_UINTEGER',SQL_CATALOG_LOCATION : 'GI_USMALLINT',
SQL_CATALOG_NAME : 'GI_YESNO',SQL_CATALOG_NAME_SEPARATOR : 'GI_STRING',SQL_CATALOG_TERM : 'GI_STRING',
SQL_CATALOG_USAGE : 'GI_UINTEGER',SQL_COLLATION_SEQ : 'GI_STRING',SQL_COLUMN_ALIAS : 'GI_YESNO',
SQL_CONCAT_NULL_BEHAVIOR : 'GI_USMALLINT',SQL_CONVERT_FUNCTIONS : 'GI_UINTEGER',SQL_CONVERT_VARCHAR : 'GI_UINTEGER',
SQL_CORRELATION_NAME : 'GI_USMALLINT',SQL_CREATE_ASSERTION : 'GI_UINTEGER',SQL_CREATE_CHARACTER_SET : 'GI_UINTEGER',
SQL_CREATE_COLLATION : 'GI_UINTEGER',SQL_CREATE_DOMAIN : 'GI_UINTEGER',SQL_CREATE_SCHEMA : 'GI_UINTEGER',
SQL_CREATE_TABLE : 'GI_UINTEGER',SQL_CREATE_TRANSLATION : 'GI_UINTEGER',SQL_CREATE_VIEW : 'GI_UINTEGER',
SQL_CURSOR_COMMIT_BEHAVIOR : 'GI_USMALLINT',SQL_CURSOR_ROLLBACK_BEHAVIOR : 'GI_USMALLINT',SQL_DATABASE_NAME : 'GI_STRING',
SQL_DATA_SOURCE_NAME : 'GI_STRING',SQL_DATA_SOURCE_READ_ONLY : 'GI_YESNO',SQL_DATETIME_LITERALS : 'GI_UINTEGER',
SQL_DBMS_NAME : 'GI_STRING',SQL_DBMS_VER : 'GI_STRING',SQL_DDL_INDEX : 'GI_UINTEGER',
SQL_DEFAULT_TXN_ISOLATION : 'GI_UINTEGER',SQL_DESCRIBE_PARAMETER : 'GI_YESNO',SQL_DM_VER : 'GI_STRING',
SQL_DRIVER_NAME : 'GI_STRING',SQL_DRIVER_ODBC_VER : 'GI_STRING',SQL_DRIVER_VER : 'GI_STRING',SQL_DROP_ASSERTION : 'GI_UINTEGER',
SQL_DROP_CHARACTER_SET : 'GI_UINTEGER', SQL_DROP_COLLATION : 'GI_UINTEGER',SQL_DROP_DOMAIN : 'GI_UINTEGER',
SQL_DROP_SCHEMA : 'GI_UINTEGER',SQL_DROP_TABLE : 'GI_UINTEGER',SQL_DROP_TRANSLATION : 'GI_UINTEGER',
SQL_DROP_VIEW : 'GI_UINTEGER',SQL_DYNAMIC_CURSOR_ATTRIBUTES1 : 'GI_UINTEGER',SQL_DYNAMIC_CURSOR_ATTRIBUTES2 : 'GI_UINTEGER',
SQL_EXPRESSIONS_IN_ORDERBY : 'GI_YESNO',SQL_FILE_USAGE : 'GI_USMALLINT',
SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 : 'GI_UINTEGER',SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 : 'GI_UINTEGER',
SQL_GETDATA_EXTENSIONS : 'GI_UINTEGER',SQL_GROUP_BY : 'GI_USMALLINT',SQL_IDENTIFIER_CASE : 'GI_USMALLINT',
SQL_IDENTIFIER_QUOTE_CHAR : 'GI_STRING',SQL_INDEX_KEYWORDS : 'GI_UINTEGER',SQL_INFO_SCHEMA_VIEWS : 'GI_UINTEGER',
SQL_INSERT_STATEMENT : 'GI_UINTEGER',SQL_INTEGRITY : 'GI_YESNO',SQL_KEYSET_CURSOR_ATTRIBUTES1 : 'GI_UINTEGER',
SQL_KEYSET_CURSOR_ATTRIBUTES2 : 'GI_UINTEGER',SQL_KEYWORDS : 'GI_STRING',
SQL_LIKE_ESCAPE_CLAUSE : 'GI_YESNO',SQL_MAX_ASYNC_CONCURRENT_STATEMENTS : 'GI_UINTEGER',
SQL_MAX_BINARY_LITERAL_LEN : 'GI_UINTEGER',SQL_MAX_CATALOG_NAME_LEN : 'GI_USMALLINT',
SQL_MAX_CHAR_LITERAL_LEN : 'GI_UINTEGER',SQL_MAX_COLUMNS_IN_GROUP_BY : 'GI_USMALLINT',
SQL_MAX_COLUMNS_IN_INDEX : 'GI_USMALLINT',SQL_MAX_COLUMNS_IN_ORDER_BY : 'GI_USMALLINT',
SQL_MAX_COLUMNS_IN_SELECT : 'GI_USMALLINT',SQL_MAX_COLUMNS_IN_TABLE : 'GI_USMALLINT',
SQL_MAX_COLUMN_NAME_LEN : 'GI_USMALLINT',SQL_MAX_CONCURRENT_ACTIVITIES : 'GI_USMALLINT',
SQL_MAX_CURSOR_NAME_LEN : 'GI_USMALLINT',SQL_MAX_DRIVER_CONNECTIONS : 'GI_USMALLINT',
SQL_MAX_IDENTIFIER_LEN : 'GI_USMALLINT',SQL_MAX_INDEX_SIZE : 'GI_UINTEGER',
SQL_MAX_PROCEDURE_NAME_LEN : 'GI_USMALLINT',SQL_MAX_ROW_SIZE : 'GI_UINTEGER',
SQL_MAX_ROW_SIZE_INCLUDES_LONG : 'GI_YESNO',SQL_MAX_SCHEMA_NAME_LEN : 'GI_USMALLINT',
SQL_MAX_STATEMENT_LEN : 'GI_UINTEGER',SQL_MAX_TABLES_IN_SELECT : 'GI_USMALLINT',
SQL_MAX_TABLE_NAME_LEN : 'GI_USMALLINT',SQL_MAX_USER_NAME_LEN : 'GI_USMALLINT',
SQL_MULTIPLE_ACTIVE_TXN : 'GI_YESNO',SQL_MULT_RESULT_SETS : 'GI_YESNO',
SQL_NEED_LONG_DATA_LEN : 'GI_YESNO',SQL_NON_NULLABLE_COLUMNS : 'GI_USMALLINT',
SQL_NULL_COLLATION : 'GI_USMALLINT',SQL_NUMERIC_FUNCTIONS : 'GI_UINTEGER',
SQL_ODBC_INTERFACE_CONFORMANCE : 'GI_UINTEGER',SQL_ODBC_VER : 'GI_STRING',SQL_OJ_CAPABILITIES : 'GI_UINTEGER',
SQL_ORDER_BY_COLUMNS_IN_SELECT : 'GI_YESNO',SQL_PARAM_ARRAY_ROW_COUNTS : 'GI_UINTEGER',
SQL_PARAM_ARRAY_SELECTS : 'GI_UINTEGER',SQL_PROCEDURES : 'GI_YESNO',SQL_PROCEDURE_TERM : 'GI_STRING',
SQL_QUOTED_IDENTIFIER_CASE : 'GI_USMALLINT',SQL_ROW_UPDATES : 'GI_YESNO',SQL_SCHEMA_TERM : 'GI_STRING',
SQL_SCHEMA_USAGE : 'GI_UINTEGER',SQL_SCROLL_OPTIONS : 'GI_UINTEGER',SQL_SEARCH_PATTERN_ESCAPE : 'GI_STRING',
SQL_SERVER_NAME : 'GI_STRING',SQL_SPECIAL_CHARACTERS : 'GI_STRING',SQL_SQL92_DATETIME_FUNCTIONS : 'GI_UINTEGER',
SQL_SQL92_FOREIGN_KEY_DELETE_RULE : 'GI_UINTEGER',SQL_SQL92_FOREIGN_KEY_UPDATE_RULE : 'GI_UINTEGER',
SQL_SQL92_GRANT : 'GI_UINTEGER',SQL_SQL92_NUMERIC_VALUE_FUNCTIONS : 'GI_UINTEGER',
SQL_SQL92_PREDICATES : 'GI_UINTEGER',SQL_SQL92_RELATIONAL_JOIN_OPERATORS : 'GI_UINTEGER',
SQL_SQL92_REVOKE : 'GI_UINTEGER',SQL_SQL92_ROW_VALUE_CONSTRUCTOR : 'GI_UINTEGER',
SQL_SQL92_STRING_FUNCTIONS : 'GI_UINTEGER',SQL_SQL92_VALUE_EXPRESSIONS : 'GI_UINTEGER',
SQL_SQL_CONFORMANCE : 'GI_UINTEGER',SQL_STANDARD_CLI_CONFORMANCE : 'GI_UINTEGER',
SQL_STATIC_CURSOR_ATTRIBUTES1 : 'GI_UINTEGER',SQL_STATIC_CURSOR_ATTRIBUTES2 : 'GI_UINTEGER',
SQL_STRING_FUNCTIONS : 'GI_UINTEGER',SQL_SUBQUERIES : 'GI_UINTEGER',
SQL_SYSTEM_FUNCTIONS : 'GI_UINTEGER',SQL_TABLE_TERM : 'GI_STRING',SQL_TIMEDATE_ADD_INTERVALS : 'GI_UINTEGER',
SQL_TIMEDATE_DIFF_INTERVALS : 'GI_UINTEGER',SQL_TIMEDATE_FUNCTIONS : 'GI_UINTEGER',
SQL_TXN_CAPABLE : 'GI_USMALLINT',SQL_TXN_ISOLATION_OPTION : 'GI_UINTEGER',
SQL_UNION : 'GI_UINTEGER',SQL_USER_NAME : 'GI_STRING',SQL_XOPEN_CLI_YEAR : 'GI_STRING',
}

#Definations for types
BINARY = bytearray
Binary = bytearray
DATETIME = datetime.datetime
Date = datetime.date
Time = datetime.time
Timestamp = datetime.datetime
STRING = str
NUMBER = float
ROWID = int
DateFromTicks = datetime.date.fromtimestamp
TimeFromTicks = lambda x: datetime.datetime.fromtimestamp(x).time()
TimestampFromTicks = datetime.datetime.fromtimestamp


#Define exceptions
class OdbcNoLibrary(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)
class OdbcLibraryError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)
class OdbcInvalidHandle(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)
class OdbcGenericError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)
class Warning(Exception):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class Error(Exception):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class InterfaceError(Error):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class DatabaseError(Error):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class InternalError(DatabaseError):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class ProgrammingError(DatabaseError):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class DataError(DatabaseError):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class IntegrityError(DatabaseError):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class NotSupportedError(Error):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)
class OperationalError(DatabaseError):
    def __init__(self, error_code, error_desc):
        self.value = (error_code, error_desc)
        self.args = (error_code, error_desc)




############################################################################
#
#       Find the ODBC library on the platform and connect to it using ctypes
#
############################################################################
# Get the References of the platform's ODBC functions via ctypes

odbc_decoding = 'utf_16'
odbc_encoding = 'utf_16_le'
ucs_length = 2

if sys.platform in ('win32','cli'):
    ODBC_API = ctypes.windll.odbc32
    # On Windows, the size of SQLWCHAR is hardcoded to 2-bytes.
    SQLWCHAR_SIZE = ctypes.sizeof(ctypes.c_ushort)
else:
    # Set load the library on linux
    try:
        # First try direct loading libodbc.so
        ODBC_API = ctypes.cdll.LoadLibrary('libodbc.so')
    except:
        # If direct loading libodbc.so failed
        # We try finding the libodbc.so by using find_library
        from ctypes.util import find_library
        library = find_library('odbc')
        if library is None:
            # If find_library still can not find the library
            # we try finding it manually from where libodbc.so usually appears
            lib_paths = ("/usr/lib/libodbc.so","/usr/lib/i386-linux-gnu/libodbc.so","/usr/lib/x86_64-linux-gnu/libodbc.so","/usr/lib/libiodbc.dylib")
            lib_paths = [path for path in lib_paths if os.path.exists(path)]
            if len(lib_paths) == 0 :
                raise OdbcNoLibrary('ODBC Library is not found. Is LD_LIBRARY_PATH set?')
            else:
                library = lib_paths[0]

        # Then we try loading the found libodbc.so again
        try:
            ODBC_API = ctypes.cdll.LoadLibrary(library)
        except:
            # If still fail loading, abort.
            raise OdbcLibraryError('Error while loading ' + library)

        # only iODBC uses utf-32 / UCS4 encoding data, others normally use utf-16 / UCS2
        # So we set those for handling.
        if 'libiodbc.dylib' in library:
            odbc_decoding = 'utf_32'
            odbc_encoding = 'utf_32_le'
            ucs_length = 4


    # unixODBC defaults to 2-bytes SQLWCHAR, unless "-DSQL_WCHART_CONVERT" was
    # added to CFLAGS, in which case it will be the size of wchar_t.
    # Note that using 4-bytes SQLWCHAR will break most ODBC drivers, as driver
    # development mostly targets the Windows platform.
    if py_v3:
        from subprocess import getstatusoutput
    else:
        from commands import getstatusoutput

    status, output = getstatusoutput('odbc_config --cflags')
    if status == 0 and 'SQL_WCHART_CONVERT' in output:
        SQLWCHAR_SIZE = ctypes.sizeof(ctypes.c_wchar)
    else:
        SQLWCHAR_SIZE = ctypes.sizeof(ctypes.c_ushort)


create_buffer_u = ctypes.create_unicode_buffer
create_buffer = ctypes.create_string_buffer
wchar_pointer = ctypes.c_wchar_p
UCS_buf = lambda s: s
def UCS_dec(buffer):
    i = 0
    uchars = []
    while True:
        uchar = buffer.raw[i:i + ucs_length].decode(odbc_decoding)
        if uchar == unicode('\x00'):
            break
        uchars.append(uchar)
        i += ucs_length
    return ''.join(uchars)
from_buffer_u = lambda buffer: buffer.value

# This is the common case on Linux, which uses wide Python build together with
# the default unixODBC without the "-DSQL_WCHART_CONVERT" CFLAGS.
if sys.platform not in ('win32','cli'):
    if UNICODE_SIZE >= SQLWCHAR_SIZE:
        # We can only use unicode buffer if the size of wchar_t (UNICODE_SIZE) is
        # the same as the size expected by the driver manager (SQLWCHAR_SIZE).
        create_buffer_u = create_buffer
        wchar_pointer = ctypes.c_char_p

        def UCS_buf(s):
            return s.encode(odbc_encoding)

        from_buffer_u = UCS_dec

    # Exoteric case, don't really care.
    elif UNICODE_SIZE < SQLWCHAR_SIZE:
        raise OdbcLibraryError('Using narrow Python build with ODBC library '
            'expecting wide unicode is not supported.')












############################################################
# Database value to Python data type mappings


SQL_TYPE_NULL = 0
SQL_DECIMAL = 3
SQL_FLOAT = 6
SQL_DATE = 9
SQL_TIME = 10
SQL_TIMESTAMP = 11
SQL_VARCHAR = 12
SQL_LONGVARCHAR = -1
SQL_VARBINARY = -3
SQL_LONGVARBINARY = -4
SQL_BIGINT = -5
SQL_WVARCHAR = -9
SQL_WLONGVARCHAR = -10
SQL_ALL_TYPES = 0
SQL_SIGNED_OFFSET = -20
SQL_SS_VARIANT = -150
SQL_SS_UDT = -151
SQL_SS_XML = -152
SQL_SS_TIME2 = -154

SQL_C_CHAR =            SQL_CHAR =          1
SQL_C_NUMERIC =         SQL_NUMERIC =       2
SQL_C_LONG =            SQL_INTEGER =       4
SQL_C_SLONG =           SQL_C_LONG + SQL_SIGNED_OFFSET
SQL_C_SHORT =           SQL_SMALLINT =      5
SQL_C_FLOAT =           SQL_REAL =          7
SQL_C_DOUBLE =          SQL_DOUBLE =        8
SQL_C_TYPE_DATE =       SQL_TYPE_DATE =     91
SQL_C_TYPE_TIME =       SQL_TYPE_TIME =     92
SQL_C_BINARY =          SQL_BINARY =        -2
SQL_C_SBIGINT =         SQL_BIGINT + SQL_SIGNED_OFFSET
SQL_C_TINYINT =         SQL_TINYINT =       -6
SQL_C_BIT =             SQL_BIT =           -7
SQL_C_WCHAR =           SQL_WCHAR =         -8
SQL_C_GUID =            SQL_GUID =          -11
SQL_C_TYPE_TIMESTAMP =  SQL_TYPE_TIMESTAMP = 93
SQL_C_DEFAULT = 99

SQL_DESC_DISPLAY_SIZE = SQL_COLUMN_DISPLAY_SIZE

def dttm_cvt(x):
    if py_v3:
        x = x.decode('ascii')
    if x == '': return None
    else: return datetime.datetime(int(x[0:4]),int(x[5:7]),int(x[8:10]),int(x[10:13]),int(x[14:16]),int(x[17:19]),int(x[20:].ljust(6,'0')))

def tm_cvt(x):
    if py_v3:
        x = x.decode('ascii')
    if x == '': return None
    else: return datetime.time(int(x[0:2]),int(x[3:5]),int(x[6:8]),int(x[9:].ljust(6,'0')))

def dt_cvt(x):
    if py_v3:
        x = x.decode('ascii')
    if x == '': return None
    else: return datetime.date(int(x[0:4]),int(x[5:7]),int(x[8:10]))

def Decimal_cvt(x):
    if py_v3:
        x = x.decode('ascii')
    return Decimal(x)

bytearray_cvt = bytearray
if sys.platform == 'cli':
    bytearray_cvt = lambda x: bytearray(buffer(x))

# Below Datatype mappings referenced the document at
# http://infocenter.sybase.com/help/index.jsp?topic=/com.sybase.help.sdk_12.5.1.aseodbc/html/aseodbc/CACFDIGH.htm

SQL_data_type_dict = { \
#SQL Data TYPE        0.Python Data Type     1.Default Output Converter  2.Buffer Type     3.Buffer Allocator   4.Default Size  5.Variable Length
SQL_TYPE_NULL       : (None,                lambda x: None,             SQL_C_CHAR,         create_buffer,      2     ,         False         ),
SQL_CHAR            : (str,                 lambda x: x,                SQL_C_CHAR,         create_buffer,      2048  ,         False         ),
SQL_NUMERIC         : (Decimal,             Decimal_cvt,                SQL_C_CHAR,         create_buffer,      150   ,         False         ),
SQL_DECIMAL         : (Decimal,             Decimal_cvt,                SQL_C_CHAR,         create_buffer,      150   ,         False         ),
SQL_INTEGER         : (int,                 int,                        SQL_C_CHAR,         create_buffer,      150   ,         False         ),
SQL_SMALLINT        : (int,                 int,                        SQL_C_CHAR,         create_buffer,      150   ,         False         ),
SQL_FLOAT           : (float,               float,                      SQL_C_CHAR,         create_buffer,      150   ,         False         ),
SQL_REAL            : (float,               float,                      SQL_C_CHAR,         create_buffer,      150   ,         False         ),
SQL_DOUBLE          : (float,               float,                      SQL_C_CHAR,         create_buffer,      200   ,         False         ),
SQL_DATE            : (datetime.date,       dt_cvt,                     SQL_C_CHAR,         create_buffer,      30    ,         False         ),
SQL_TIME            : (datetime.time,       tm_cvt,                     SQL_C_CHAR,         create_buffer,      20    ,         False         ),
SQL_SS_TIME2        : (datetime.time,       tm_cvt,                     SQL_C_CHAR,         create_buffer,      20    ,         False         ),
SQL_TIMESTAMP       : (datetime.datetime,   dttm_cvt,                   SQL_C_CHAR,         create_buffer,      30    ,         False         ),
SQL_VARCHAR         : (str,                 lambda x: x,                SQL_C_CHAR,         create_buffer,      2048  ,         False         ),
SQL_LONGVARCHAR     : (str,                 lambda x: x,                SQL_C_CHAR,         create_buffer,      20500 ,         True          ),
SQL_BINARY          : (bytearray,           bytearray_cvt,              SQL_C_BINARY,       create_buffer,      5120  ,         True          ),
SQL_VARBINARY       : (bytearray,           bytearray_cvt,              SQL_C_BINARY,       create_buffer,      5120  ,         True          ),
SQL_LONGVARBINARY   : (bytearray,           bytearray_cvt,              SQL_C_BINARY,       create_buffer,      20500 ,         True          ),
SQL_BIGINT          : (long,                long,                       SQL_C_CHAR,         create_buffer,      150   ,         False         ),
SQL_TINYINT         : (int,                 int,                        SQL_C_CHAR,         create_buffer,      150   ,         False         ),
SQL_BIT             : (bool,                lambda x:x == BYTE_1,       SQL_C_CHAR,         create_buffer,      2     ,         False         ),
SQL_WCHAR           : (unicode,             lambda x: x,                SQL_C_WCHAR,        create_buffer_u,    2048  ,         False          ),
SQL_WVARCHAR        : (unicode,             lambda x: x,                SQL_C_WCHAR,        create_buffer_u,    2048  ,         False          ),
SQL_GUID            : (str,                 str,                        SQL_C_CHAR,         create_buffer,      2048  ,         False         ),
SQL_WLONGVARCHAR    : (unicode,             lambda x: x,                SQL_C_WCHAR,        create_buffer_u,    20500 ,         True          ),
SQL_TYPE_DATE       : (datetime.date,       dt_cvt,                     SQL_C_CHAR,         create_buffer,      30    ,         False         ),
SQL_TYPE_TIME       : (datetime.time,       tm_cvt,                     SQL_C_CHAR,         create_buffer,      20    ,         False         ),
SQL_TYPE_TIMESTAMP  : (datetime.datetime,   dttm_cvt,                   SQL_C_CHAR,         create_buffer,      30    ,         False         ),
SQL_SS_VARIANT      : (str,                 lambda x: x,                SQL_C_CHAR,         create_buffer,      2048  ,         True         ),
SQL_SS_XML          : (unicode,             lambda x: x,                SQL_C_WCHAR,        create_buffer_u,    20500 ,         True          ),
SQL_SS_UDT          : (bytearray,           bytearray_cvt,              SQL_C_BINARY,       create_buffer,      5120  ,         True          ),
}


"""
Types mapping, applicable for 32-bit and 64-bit Linux / Windows / Mac OS X.

SQLPointer -> ctypes.c_void_p
SQLCHAR * -> ctypes.c_char_p
SQLWCHAR * -> ctypes.c_wchar_p on Windows, ctypes.c_char_p with unixODBC
SQLINT -> ctypes.c_int
SQLSMALLINT -> ctypes.c_short
SQMUSMALLINT -> ctypes.c_ushort
SQLLEN -> ctypes.c_ssize_t
SQLULEN -> ctypes.c_size_t
SQLRETURN -> ctypes.c_short
"""

# Define the python return type for ODBC functions with ret result.
funcs_with_ret = [
    "SQLAllocHandle",
    "SQLBindParameter",
    "SQLBindCol",
    "SQLCloseCursor",
    "SQLColAttribute",
    "SQLColumns",
    "SQLColumnsW",
    "SQLConnect",
    "SQLConnectW",
    "SQLDataSources",
    "SQLDataSourcesW",
    "SQLDescribeCol",
    "SQLDescribeColW",
    "SQLDescribeParam",
    "SQLDisconnect",
    "SQLDriverConnect",
    "SQLDriverConnectW",
    "SQLDrivers",
    "SQLDriversW",
    "SQLEndTran",
    "SQLExecDirect",
    "SQLExecDirectW",
    "SQLExecute",
    "SQLFetch",
    "SQLFetchScroll",
    "SQLForeignKeys",
    "SQLForeignKeysW",
    "SQLFreeHandle",
    "SQLFreeStmt",
    "SQLGetData",
    "SQLGetDiagRec",
    "SQLGetDiagRecW",
    "SQLGetInfo",
    "SQLGetInfoW",
    "SQLGetTypeInfo",
    "SQLMoreResults",
    "SQLNumParams",
    "SQLNumResultCols",
    "SQLPrepare",
    "SQLPrepareW",
    "SQLPrimaryKeys",
    "SQLPrimaryKeysW",
    "SQLProcedureColumns",
    "SQLProcedureColumnsW",
    "SQLProcedures",
    "SQLProceduresW",
    "SQLRowCount",
    "SQLSetConnectAttr",
    "SQLSetEnvAttr",
    "SQLStatistics",
    "SQLStatisticsW",
    "SQLTables",
    "SQLTablesW",
]

for func_name in funcs_with_ret:
    getattr(ODBC_API, func_name).restype = ctypes.c_short

if sys.platform not in ('cli'):
    #Seems like the IronPython can not declare ctypes.POINTER type arguments
    ODBC_API.SQLAllocHandle.argtypes = [
        ctypes.c_short, ctypes.c_void_p, ctypes.POINTER(ctypes.c_void_p),
    ]

    ODBC_API.SQLBindParameter.argtypes = [
        ctypes.c_void_p, ctypes.c_ushort, ctypes.c_short,
        ctypes.c_short,  ctypes.c_short,  ctypes.c_size_t,
        ctypes.c_short,  ctypes.c_void_p, ctypes.c_ssize_t, ctypes.POINTER(ctypes.c_ssize_t),
    ]

    ODBC_API.SQLColAttribute.argtypes = [
        ctypes.c_void_p, ctypes.c_ushort, ctypes.c_ushort,
        ctypes.c_void_p, ctypes.c_short, ctypes.POINTER(ctypes.c_short), ctypes.POINTER(ctypes.c_ssize_t),
    ]

    ODBC_API.SQLDataSources.argtypes = [
        ctypes.c_void_p, ctypes.c_ushort, ctypes.c_char_p,
        ctypes.c_short, ctypes.POINTER(ctypes.c_short),
        ctypes.c_char_p,  ctypes.c_short, ctypes.POINTER(ctypes.c_short),
    ]

    ODBC_API.SQLDescribeCol.argtypes = [
        ctypes.c_void_p, ctypes.c_ushort, ctypes.c_char_p, ctypes.c_short,
        ctypes.POINTER(ctypes.c_short), ctypes.POINTER(ctypes.c_short),
        ctypes.POINTER(ctypes.c_size_t), ctypes.POINTER(ctypes.c_short), ctypes.POINTER(ctypes.c_short),
    ]

    ODBC_API.SQLDescribeParam.argtypes = [
        ctypes.c_void_p, ctypes.c_ushort,
        ctypes.POINTER(ctypes.c_short), ctypes.POINTER(ctypes.c_size_t),
        ctypes.POINTER(ctypes.c_short), ctypes.POINTER(ctypes.c_short),
    ]

    ODBC_API.SQLDriverConnect.argtypes = [
        ctypes.c_void_p, ctypes.c_void_p, ctypes.c_char_p,
        ctypes.c_short, ctypes.c_char_p, ctypes.c_short,
        ctypes.POINTER(ctypes.c_short), ctypes.c_ushort,
    ]

    ODBC_API.SQLDrivers.argtypes = [
        ctypes.c_void_p, ctypes.c_ushort,
        ctypes.c_char_p, ctypes.c_short, ctypes.POINTER(ctypes.c_short),
        ctypes.c_char_p, ctypes.c_short, ctypes.POINTER(ctypes.c_short),
    ]

    ODBC_API.SQLGetData.argtypes = [
        ctypes.c_void_p, ctypes.c_ushort, ctypes.c_short,
        ctypes.c_void_p, ctypes.c_ssize_t, ctypes.POINTER(ctypes.c_ssize_t),
    ]

    ODBC_API.SQLGetDiagRec.argtypes = [
        ctypes.c_short, ctypes.c_void_p, ctypes.c_short,
        ctypes.c_char_p, ctypes.POINTER(ctypes.c_int),
        ctypes.c_char_p, ctypes.c_short, ctypes.POINTER(ctypes.c_short),
    ]

    ODBC_API.SQLGetInfo.argtypes = [
        ctypes.c_void_p, ctypes.c_ushort, ctypes.c_void_p,
        ctypes.c_short, ctypes.POINTER(ctypes.c_short),
    ]

    ODBC_API.SQLRowCount.argtypes = [
        ctypes.c_void_p, ctypes.POINTER(ctypes.c_ssize_t),
    ]

    ODBC_API.SQLNumParams.argtypes = [
        ctypes.c_void_p, ctypes.POINTER(ctypes.c_short),
    ]

    ODBC_API.SQLNumResultCols.argtypes = [
        ctypes.c_void_p, ctypes.POINTER(ctypes.c_short),
    ]


ODBC_API.SQLCloseCursor.argtypes = [ctypes.c_void_p]

ODBC_API.SQLColumns.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p,
    ctypes.c_short, ctypes.c_char_p, ctypes.c_short,
]

ODBC_API.SQLConnect.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p, ctypes.c_short,
]

ODBC_API.SQLDisconnect.argtypes = [ctypes.c_void_p]

ODBC_API.SQLEndTran.argtypes = [
    ctypes.c_short, ctypes.c_void_p, ctypes.c_short,
]

ODBC_API.SQLExecute.argtypes = [ctypes.c_void_p]

ODBC_API.SQLExecDirect.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int,
]

ODBC_API.SQLFetch.argtypes = [ctypes.c_void_p]

ODBC_API.SQLFetchScroll.argtypes = [
    ctypes.c_void_p, ctypes.c_short, ctypes.c_ssize_t,
]

ODBC_API.SQLForeignKeys.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p,
    ctypes.c_short, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p, ctypes.c_short,
]

ODBC_API.SQLFreeHandle.argtypes = [
    ctypes.c_short, ctypes.c_void_p,
]

ODBC_API.SQLFreeStmt.argtypes = [
    ctypes.c_void_p, ctypes.c_ushort,
]


ODBC_API.SQLGetTypeInfo.argtypes = [
    ctypes.c_void_p, ctypes.c_short,
]

ODBC_API.SQLMoreResults.argtypes = [ctypes.c_void_p]


ODBC_API.SQLPrepare.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int,
]

ODBC_API.SQLPrimaryKeys.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p, ctypes.c_short,
]

ODBC_API.SQLProcedureColumns.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p,
    ctypes.c_short, ctypes.c_char_p, ctypes.c_short,
]

ODBC_API.SQLProcedures.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p, ctypes.c_short,
]


ODBC_API.SQLSetConnectAttr.argtypes = [
    ctypes.c_void_p, ctypes.c_int, ctypes.c_void_p, ctypes.c_int,
]

ODBC_API.SQLSetEnvAttr.argtypes = [
    ctypes.c_void_p, ctypes.c_int, ctypes.c_void_p,  ctypes.c_int,
]

ODBC_API.SQLStatistics.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p,
    ctypes.c_short, ctypes.c_ushort, ctypes.c_ushort,
]

ODBC_API.SQLTables.argtypes = [
    ctypes.c_void_p, ctypes.c_char_p, ctypes.c_short,
    ctypes.c_char_p, ctypes.c_short, ctypes.c_char_p,
    ctypes.c_short, ctypes.c_char_p,  ctypes.c_short,
]

def to_wchar(argtypes):
    if argtypes: # Under IronPython some argtypes are not declared
        result = []
        for x in argtypes:
            if x == ctypes.c_char_p:
                result.append(wchar_pointer)
            else:
                result.append(x)
        return result
    else:
        return argtypes

ODBC_API.SQLColumnsW.argtypes = to_wchar(ODBC_API.SQLColumns.argtypes)
ODBC_API.SQLConnectW.argtypes = to_wchar(ODBC_API.SQLConnect.argtypes)
ODBC_API.SQLDataSourcesW.argtypes = to_wchar(ODBC_API.SQLDataSources.argtypes)
ODBC_API.SQLDescribeColW.argtypes = to_wchar(ODBC_API.SQLDescribeCol.argtypes)
ODBC_API.SQLDriverConnectW.argtypes = to_wchar(ODBC_API.SQLDriverConnect.argtypes)
ODBC_API.SQLDriversW.argtypes = to_wchar(ODBC_API.SQLDrivers.argtypes)
ODBC_API.SQLExecDirectW.argtypes = to_wchar(ODBC_API.SQLExecDirect.argtypes)
ODBC_API.SQLForeignKeysW.argtypes = to_wchar(ODBC_API.SQLForeignKeys.argtypes)
ODBC_API.SQLPrepareW.argtypes = to_wchar(ODBC_API.SQLPrepare.argtypes)
ODBC_API.SQLPrimaryKeysW.argtypes = to_wchar(ODBC_API.SQLPrimaryKeys.argtypes)
ODBC_API.SQLProcedureColumnsW.argtypes = to_wchar(ODBC_API.SQLProcedureColumns.argtypes)
ODBC_API.SQLProceduresW.argtypes = to_wchar(ODBC_API.SQLProcedures.argtypes)
ODBC_API.SQLStatisticsW.argtypes = to_wchar(ODBC_API.SQLStatistics.argtypes)
ODBC_API.SQLTablesW.argtypes = to_wchar(ODBC_API.SQLTables.argtypes)
ODBC_API.SQLGetDiagRecW.argtypes = to_wchar(ODBC_API.SQLGetDiagRec.argtypes)
ODBC_API.SQLGetInfoW.argtypes = to_wchar(ODBC_API.SQLGetInfo.argtypes)

# Set the alias for the ctypes functions for beter code readbility or performance.
ADDR = ctypes.byref
c_short = ctypes.c_short
c_ssize_t = ctypes.c_ssize_t
SQLFetch = ODBC_API.SQLFetch
SQLExecute = ODBC_API.SQLExecute
SQLBindParameter = ODBC_API.SQLBindParameter
SQLGetData = ODBC_API.SQLGetData
SQLRowCount = ODBC_API.SQLRowCount
SQLNumResultCols = ODBC_API.SQLNumResultCols
SQLEndTran = ODBC_API.SQLEndTran
# Set alias for beter code readbility or performance.
NO_FREE_STATEMENT = 0
FREE_STATEMENT = 1
BLANK_BYTE = str_8b()

def ctrl_err(ht, h, val_ret, ansi):
    """Classify type of ODBC error from (type of handle, handle, return value)
    , and raise with a list"""

    if ansi:
        state = create_buffer(22)
        Message = create_buffer(1024*4)
        ODBC_func = ODBC_API.SQLGetDiagRec
        if py_v3:
            raw_s = lambda s: bytes(s,'ascii')
        else:
            raw_s = str_8b
    else:
        state = create_buffer_u(22)
        Message = create_buffer_u(1024*4)
        ODBC_func = ODBC_API.SQLGetDiagRecW
        raw_s = unicode
    NativeError = ctypes.c_int()
    Buffer_len = c_short()
    err_list = []
    number_errors = 1

    while 1:
        ret = ODBC_func(ht, h, number_errors, state, \
            ADDR(NativeError), Message, 1024, ADDR(Buffer_len))
        if ret == SQL_NO_DATA_FOUND:
            #No more data, I can raise
            #print(err_list[0][1])
            state = err_list[0][0]
            err_text = raw_s('[')+state+raw_s('] ')+err_list[0][1]
            if state[:2] in (raw_s('24'),raw_s('25'),raw_s('42')):
                raise ProgrammingError(state,err_text)
            elif state[:2] in (raw_s('22')):
                raise DataError(state,err_text)
            elif state[:2] in (raw_s('23')) or state == raw_s('40002'):
                raise IntegrityError(state,err_text)
            elif state == raw_s('0A000'):
                raise NotSupportedError(state,err_text)
            elif state in (raw_s('HYT00'),raw_s('HYT01')):
                raise OperationalError(state,err_text)
            elif state[:2] in (raw_s('IM'),raw_s('HY')):
                raise Error(state,err_text)
            else:
                raise DatabaseError(state,err_text)
            break
        elif ret == SQL_INVALID_HANDLE:
            #The handle passed is an invalid handle
            raise ProgrammingError('', 'SQL_INVALID_HANDLE')
        elif ret == SQL_SUCCESS:
            if ansi:
                err_list.append((state.value, Message.value, NativeError.value))
            else:
                err_list.append((from_buffer_u(state), from_buffer_u(Message), NativeError.value))
            number_errors += 1
        elif ret == SQL_ERROR:
            raise ProgrammingError('', 'SQL_ERROR')



def check_success(ODBC_obj, ret):
    """ Validate return value, if not success, raise exceptions based on the handle """
    if ret not in (SQL_SUCCESS, SQL_SUCCESS_WITH_INFO, SQL_NO_DATA):
        if isinstance(ODBC_obj, Cursor):
            ctrl_err(SQL_HANDLE_STMT, ODBC_obj.stmt_h, ret, ODBC_obj.ansi)
        elif isinstance(ODBC_obj, Connection):
            ctrl_err(SQL_HANDLE_DBC, ODBC_obj.dbc_h, ret, ODBC_obj.ansi)
        else:
            ctrl_err(SQL_HANDLE_ENV, ODBC_obj, ret, False)


def AllocateEnv():
    if pooling:
        ret = ODBC_API.SQLSetEnvAttr(SQL_NULL_HANDLE, SQL_ATTR_CONNECTION_POOLING, SQL_CP_ONE_PER_HENV, SQL_IS_UINTEGER)
        check_success(SQL_NULL_HANDLE, ret)

    '''
    Allocate an ODBC environment by initializing the handle shared_env_h
    ODBC enviroment needed to be created, so connections can be created under it
    connections pooling can be shared under one environment
    '''
    global shared_env_h
    shared_env_h  = ctypes.c_void_p()
    ret = ODBC_API.SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, ADDR(shared_env_h))
    check_success(shared_env_h, ret)

    # Set the ODBC environment's compatibil leve to ODBC 3.0
    ret = ODBC_API.SQLSetEnvAttr(shared_env_h, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, 0)
    check_success(shared_env_h, ret)


"""
Here, we have a few callables that determine how a result row is returned.

A new one can be added by creating a callable that:
- accepts a cursor as its parameter.
- returns a callable that accepts an iterable containing the row values.
"""

def TupleRow(cursor):
    """Normal tuple with added attribute `cursor_description`, as in pyodbc.

    This is the default.
    """
    class Row(tuple):
        cursor_description = cursor.description

        def get(self, field):
            if not hasattr(self, 'field_dict'):
                self.field_dict = {}
                for i,item in enumerate(self):
                    self.field_dict[self.cursor_description[i][0]] = item
            return self.field_dict.get(field)

        def __getitem__(self, field):
            if isinstance(field, (unicode,str)):
                return self.get(field)
            else:
                return tuple.__getitem__(self,field)

    return Row


def NamedTupleRow(cursor):
    """Named tuple to allow attribute lookup by name.

    Requires py2.6 or above.
    """
    from collections import namedtuple

    attr_names = [x[0] for x in cursor._ColBufferList]

    class Row(namedtuple('Row', attr_names, rename=True)):
        cursor_description = cursor.description

        def __new__(cls, iterable):
            return super(Row, cls).__new__(cls, *iterable)

    return Row


def MutableNamedTupleRow(cursor):
    """Mutable named tuple to allow attribute to be replaced. This should be
    compatible with pyodbc's Row type.

    Requires 3rd-party library "recordtype".
    """
    from recordtype import recordtype

    attr_names = [x[0] for x in cursor._ColBufferList]

    class Row(recordtype('Row', attr_names, rename=True)):
        cursor_description = cursor.description

        def __init__(self, iterable):
            super(Row, self).__init__(*iterable)

        def __iter__(self):
            for field_name in self.__slots__:
                yield getattr(self, field_name)

        def __getitem__(self, index):
            if isinstance(index, slice):
                return tuple(getattr(self, x) for x in self.__slots__[index])
            return getattr(self, self.__slots__[index])

        def __setitem__(self, index, value):
            setattr(self, self.__slots__[index], value)

    return Row

# When Null is used in a binary parameter, database usually would not
# accept the None for a binary field, so the work around is to use a
# Specical None that the pypyodbc moudle would know this NULL is for
# a binary field.
class BinaryNullType(): pass
BinaryNull = BinaryNullType()

# The get_type function is used to determine if parameters need to be re-binded
# against the changed parameter types
# 'b' for bool, 'U' for long unicode string, 'u' for short unicode string
# 'S' for long 8 bit string, 's' for short 8 bit string, 'l' for big integer, 'i' for normal integer
# 'f' for float, 'D' for Decimal, 't' for datetime.time, 'd' for datetime.datetime, 'dt' for datetime.datetime
# 'bi' for binary
def get_type(v):

    if isinstance(v, bool):
        return ('b',)
    elif isinstance(v, unicode):
        if len(v) >= 255:
            return  ('U',(len(v)//1000+1)*1000)
        else:
            return ('u',)
    elif isinstance(v, (str_8b,str)):
        if len(v) >= 255:
            return  ('S',(len(v)//1000+1)*1000)
        else:
            return  ('s',)
    elif isinstance(v, (int, long)):
        #SQL_BIGINT defination: http://msdn.microsoft.com/en-us/library/ms187745.aspx
        if v > 2147483647 or v < -2147483648:
            return  ('l',)
        else:
            return  ('i',)
    elif isinstance(v, float):
        return ('f',)
    elif isinstance(v, BinaryNullType):
        return ('BN',)
    elif v is None:
        return ('N',)
    elif isinstance(v, Decimal):
        t = v.as_tuple() #1.23 -> (1,2,3),-2 , 1.23*E7 -> (1,2,3),5
        return  ('D',(len(t[1]),0 - t[2])) # number of digits, and number of decimal digits


    elif isinstance (v, datetime.datetime):
        return ('dt',)
    elif isinstance (v, datetime.date):
        return ('d',)
    elif isinstance(v, datetime.time):
        return ('t',)
    elif isinstance (v, (bytearray, buffer)):
        return ('bi',(len(v)//1000+1)*1000)

    return type(v)


# The Cursor Class.
class Cursor:
    def __init__(self, conx, row_type_callable=None):
        """ Initialize self.stmt_h, which is the handle of a statement
        A statement is actually the basis of a python"cursor" object
        """
        self.stmt_h = ctypes.c_void_p()
        self.connection = conx
        self.ansi = conx.ansi
        self.row_type_callable = row_type_callable or TupleRow
        self.statement = None
        self._last_param_types = None
        self._ParamBufferList = []
        self._ColBufferList = []
        self._row_type = None
        self._buf_cvt_func = []
        self.rowcount = -1
        self.description = None
        self.autocommit = None
        self._ColTypeCodeList = []
        self._outputsize = {}
        self._inputsizers = []
        self.arraysize = 1
        ret = ODBC_API.SQLAllocHandle(SQL_HANDLE_STMT, self.connection.dbc_h, ADDR(self.stmt_h))
        check_success(self, ret)
        self._PARAM_SQL_TYPE_LIST = []
        self.closed = False


    def prepare(self, query_string):
        """prepare a query"""

        #self._free_results(FREE_STATEMENT)
        if not self.connection:
            self.close()

        if type(query_string) == unicode:
            c_query_string = wchar_pointer(UCS_buf(query_string))
            ret = ODBC_API.SQLPrepareW(self.stmt_h, c_query_string, len(query_string))
        else:
            c_query_string = ctypes.c_char_p(query_string)
            ret = ODBC_API.SQLPrepare(self.stmt_h, c_query_string, len(query_string))
        if ret != SQL_SUCCESS:
            check_success(self, ret)


        self._PARAM_SQL_TYPE_LIST = []

        if self.connection.support_SQLDescribeParam:
            # SQLServer's SQLDescribeParam only supports DML SQL, so avoid the SELECT statement
            if True:# 'SELECT' not in query_string.upper():
                #self._free_results(NO_FREE_STATEMENT)
                NumParams = c_short()
                ret = ODBC_API.SQLNumParams(self.stmt_h, ADDR(NumParams))
                if ret != SQL_SUCCESS:
                    check_success(self, ret)

                for col_num in range(NumParams.value):
                    ParameterNumber = ctypes.c_ushort(col_num + 1)
                    DataType = c_short()
                    ParameterSize = ctypes.c_size_t()
                    DecimalDigits = c_short()
                    Nullable = c_short()
                    ret = ODBC_API.SQLDescribeParam(
                        self.stmt_h,
                        ParameterNumber,
                        ADDR(DataType),
                        ADDR(ParameterSize),
                        ADDR(DecimalDigits),
                        ADDR(Nullable),
                    )
                    if ret != SQL_SUCCESS:
                        try:
                            check_success(self, ret)
                        except DatabaseError:
                            if sys.exc_info()[1].value[0] == '07009':
                                self._PARAM_SQL_TYPE_LIST = []
                                break
                            else:
                                raise sys.exc_info()[1]
                        except:
                            raise sys.exc_info()[1]

                    self._PARAM_SQL_TYPE_LIST.append((DataType.value,DecimalDigits.value))

        self.statement = query_string


    def _BindParams(self, param_types, pram_io_list = []):
        """Create parameter buffers based on param types, and bind them to the statement"""
        # Clear the old Parameters
        if not self.connection:
            self.close()
        #self._free_results(NO_FREE_STATEMENT)

        # Get the number of query parameters judged by database.
        NumParams = c_short()
        ret = ODBC_API.SQLNumParams(self.stmt_h, ADDR(NumParams))
        if ret != SQL_SUCCESS:
            check_success(self, ret)

        if len(param_types) != NumParams.value:
            # In case number of parameters provided do not same as number required
            error_desc = "The SQL contains %d parameter markers, but %d parameters were supplied" \
                        %(NumParams.value,len(param_types))
            raise ProgrammingError('HY000',error_desc)


        # Every parameter needs to be binded to a buffer
        ParamBufferList = []
        # Temporary holder since we can only call SQLDescribeParam before
        # calling SQLBindParam.
        temp_holder = []
        for col_num in range(NumParams.value):
            dec_num = 0
            buf_size = 512

            if param_types[col_num][0] == 'u':
                sql_c_type = SQL_C_WCHAR
                sql_type = SQL_WVARCHAR
                buf_size = 255
                ParameterBuffer = create_buffer_u(buf_size)

            elif param_types[col_num][0] == 's':
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_VARCHAR
                buf_size = 255
                ParameterBuffer = create_buffer(buf_size)


            elif param_types[col_num][0] == 'U':
                sql_c_type = SQL_C_WCHAR
                sql_type = SQL_WLONGVARCHAR
                buf_size = param_types[col_num][1]#len(self._inputsizers)>col_num and self._inputsizers[col_num] or 20500
                ParameterBuffer = create_buffer_u(buf_size)

            elif param_types[col_num][0] == 'S':
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_LONGVARCHAR
                buf_size = param_types[col_num][1]#len(self._inputsizers)>col_num and self._inputsizers[col_num] or 20500
                ParameterBuffer = create_buffer(buf_size)

            # bool subclasses int, thus has to go first
            elif param_types[col_num][0] == 'b':
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_BIT
                buf_size = SQL_data_type_dict[sql_type][4]
                ParameterBuffer = create_buffer(buf_size)

            elif param_types[col_num][0] == 'i':
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_INTEGER
                buf_size = SQL_data_type_dict[sql_type][4]
                ParameterBuffer = create_buffer(buf_size)

            elif param_types[col_num][0] == 'l':
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_BIGINT
                buf_size = SQL_data_type_dict[sql_type][4]
                ParameterBuffer = create_buffer(buf_size)


            elif param_types[col_num][0] == 'D': #Decimal
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_NUMERIC
                digit_num, dec_num = param_types[col_num][1]
                if dec_num > 0:
                    # has decimal
                    buf_size = digit_num
                    dec_num = dec_num
                else:
                    # no decimal
                    buf_size = digit_num - dec_num
                    dec_num = 0

                ParameterBuffer = create_buffer(buf_size + 4)# add extra length for sign and dot


            elif param_types[col_num][0] == 'f':
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_DOUBLE
                buf_size = SQL_data_type_dict[sql_type][4]
                ParameterBuffer = create_buffer(buf_size)


            # datetime subclasses date, thus has to go first
            elif param_types[col_num][0] == 'dt':
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_TYPE_TIMESTAMP
                buf_size = self.connection.type_size_dic[SQL_TYPE_TIMESTAMP][0]
                ParameterBuffer = create_buffer(buf_size)
                dec_num = self.connection.type_size_dic[SQL_TYPE_TIMESTAMP][1]


            elif param_types[col_num][0] == 'd':
                sql_c_type = SQL_C_CHAR
                if SQL_TYPE_DATE in self.connection.type_size_dic:
                    #if DEBUG:print('conx.type_size_dic.has_key(SQL_TYPE_DATE)')
                    sql_type = SQL_TYPE_DATE
                    buf_size = self.connection.type_size_dic[SQL_TYPE_DATE][0]

                    ParameterBuffer = create_buffer(buf_size)
                    dec_num = self.connection.type_size_dic[SQL_TYPE_DATE][1]

                else:
                    # SQL Sever <2008 doesn't have a DATE type.
                    sql_type = SQL_TYPE_TIMESTAMP
                    buf_size = 10
                    ParameterBuffer = create_buffer(buf_size)


            elif param_types[col_num][0] == 't':
                sql_c_type = SQL_C_CHAR
                if SQL_TYPE_TIME in self.connection.type_size_dic:
                    sql_type = SQL_TYPE_TIME
                    buf_size = self.connection.type_size_dic[SQL_TYPE_TIME][0]
                    ParameterBuffer = create_buffer(buf_size)
                    dec_num = self.connection.type_size_dic[SQL_TYPE_TIME][1]
                elif SQL_SS_TIME2 in self.connection.type_size_dic:
                    # TIME type added in SQL Server 2008
                    sql_type = SQL_SS_TIME2
                    buf_size = self.connection.type_size_dic[SQL_SS_TIME2][0]
                    ParameterBuffer = create_buffer(buf_size)
                    dec_num = self.connection.type_size_dic[SQL_SS_TIME2][1]
                else:
                    # SQL Sever <2008 doesn't have a TIME type.
                    sql_type = SQL_TYPE_TIMESTAMP
                    buf_size = self.connection.type_size_dic[SQL_TYPE_TIMESTAMP][0]
                    ParameterBuffer = create_buffer(buf_size)
                    dec_num = 3

            elif param_types[col_num][0] == 'BN':
                sql_c_type = SQL_C_BINARY
                sql_type = SQL_VARBINARY
                buf_size = 1
                ParameterBuffer = create_buffer(buf_size)

            elif param_types[col_num][0] == 'N':
                if len(self._PARAM_SQL_TYPE_LIST) > 0:
                    sql_c_type = SQL_C_DEFAULT
                    sql_type = self._PARAM_SQL_TYPE_LIST[col_num][0]
                    buf_size = 1
                    ParameterBuffer = create_buffer(buf_size)
                else:
                    sql_c_type = SQL_C_CHAR
                    sql_type = SQL_CHAR
                    buf_size = 1
                    ParameterBuffer = create_buffer(buf_size)
            elif param_types[col_num][0] == 'bi':
                sql_c_type = SQL_C_BINARY
                sql_type = SQL_LONGVARBINARY
                buf_size = param_types[col_num][1]#len(self._inputsizers)>col_num and self._inputsizers[col_num] or 20500
                ParameterBuffer = create_buffer(buf_size)


            else:
                sql_c_type = SQL_C_CHAR
                sql_type = SQL_LONGVARCHAR
                buf_size = len(self._inputsizers)>col_num and self._inputsizers[col_num] or 20500
                ParameterBuffer = create_buffer(buf_size)

            temp_holder.append((sql_c_type, sql_type, buf_size, dec_num, ParameterBuffer))

        for col_num, (sql_c_type, sql_type, buf_size, dec_num, ParameterBuffer) in enumerate(temp_holder):
            BufferLen = c_ssize_t(buf_size)
            LenOrIndBuf = c_ssize_t()


            InputOutputType = SQL_PARAM_INPUT
            if len(pram_io_list) > col_num:
                InputOutputType = pram_io_list[col_num]

            ret = SQLBindParameter(self.stmt_h, col_num + 1, InputOutputType, sql_c_type, sql_type, buf_size,\
                    dec_num, ADDR(ParameterBuffer), BufferLen,ADDR(LenOrIndBuf))
            if ret != SQL_SUCCESS:
                check_success(self, ret)
            # Append the value buffer and the length buffer to the array
            ParamBufferList.append((ParameterBuffer,LenOrIndBuf,sql_type))

        self._last_param_types = param_types
        self._ParamBufferList = ParamBufferList


    def execute(self, query_string, params=None, many_mode=False, call_mode=False):
        """ Execute the query string, with optional parameters.
        If parameters are provided, the query would first be prepared, then executed with parameters;
        If parameters are not provided, only th query sting, it would be executed directly
        """
        if not self.connection:
            self.close()

        self._free_stmt(SQL_CLOSE)
        if params:
            # If parameters exist, first prepare the query then executed with parameters

            if not isinstance(params, (tuple, list)):
                raise TypeError("Params must be in a list, tuple, or Row")


            if query_string != self.statement:
                # if the query is not same as last query, then it is not prepared
                self.prepare(query_string)


            param_types = list(map(get_type, params))

            if call_mode:
                self._free_stmt(SQL_RESET_PARAMS)
                self._BindParams(param_types, self._pram_io_list)
            else:
                if self._last_param_types is None:
                    self._free_stmt(SQL_RESET_PARAMS)
                    self._BindParams(param_types)
                elif len(param_types) != len(self._last_param_types):
                    self._free_stmt(SQL_RESET_PARAMS)
                    self._BindParams(param_types)
                elif sum([p_type[0] != 'N' and p_type != self._last_param_types[i] for i,p_type in enumerate(param_types)]) > 0:
                    self._free_stmt(SQL_RESET_PARAMS)
                    self._BindParams(param_types)


            # With query prepared, now put parameters into buffers
            col_num = 0
            for param_buffer, param_buffer_len, sql_type in self._ParamBufferList:
                c_char_buf, c_buf_len = '', 0
                param_val = params[col_num]
                if param_types[col_num][0] in ('N','BN'):
                    param_buffer_len.value = SQL_NULL_DATA
                    col_num += 1
                    continue
                elif param_types[col_num][0] in ('i','l','f'):
                    if py_v3:
                        c_char_buf = bytes(str(param_val),'ascii')
                    else:
                        c_char_buf = str(param_val)
                    c_buf_len = len(c_char_buf)

                elif param_types[col_num][0] in ('s','S'):
                    c_char_buf = param_val
                    c_buf_len = len(c_char_buf)
                elif param_types[col_num][0] in ('u','U'):
                    c_char_buf = UCS_buf(param_val)
                    c_buf_len = len(c_char_buf)

                elif param_types[col_num][0] == 'dt':
                    max_len = self.connection.type_size_dic[SQL_TYPE_TIMESTAMP][0]
                    datetime_str = param_val.strftime('%Y-%m-%d %H:%M:%S.%f')
                    c_char_buf = datetime_str[:max_len]
                    if py_v3:
                        c_char_buf = bytes(c_char_buf,'ascii')

                    c_buf_len = len(c_char_buf)
                    # print c_buf_len, c_char_buf

                elif param_types[col_num][0] == 'd':
                    if SQL_TYPE_DATE in self.connection.type_size_dic:
                        max_len = self.connection.type_size_dic[SQL_TYPE_DATE][0]
                    else:
                        max_len = 10
                    c_char_buf = param_val.isoformat()[:max_len]
                    if py_v3:
                        c_char_buf = bytes(c_char_buf,'ascii')
                    c_buf_len = len(c_char_buf)
                    #print c_char_buf

                elif param_types[col_num][0] == 't':
                    if SQL_TYPE_TIME in self.connection.type_size_dic:
                        max_len = self.connection.type_size_dic[SQL_TYPE_TIME][0]
                        c_char_buf = param_val.isoformat()[:max_len]
                        c_buf_len = len(c_char_buf)
                    elif SQL_SS_TIME2 in self.connection.type_size_dic:
                        max_len = self.connection.type_size_dic[SQL_SS_TIME2][0]
                        c_char_buf = param_val.isoformat()[:max_len]
                        c_buf_len = len(c_char_buf)
                    else:
                        c_buf_len = self.connection.type_size_dic[SQL_TYPE_TIMESTAMP][0]
                        time_str = param_val.isoformat()
                        if len(time_str) == 8:
                            time_str += '.000'
                        c_char_buf = '1900-01-01 '+time_str[0:c_buf_len - 11]
                    if py_v3:
                        c_char_buf = bytes(c_char_buf,'ascii')
                    #print c_buf_len, c_char_buf

                elif param_types[col_num][0] == 'b':
                    if param_val == True:
                        c_char_buf = '1'
                    else:
                        c_char_buf = '0'
                    if py_v3:
                        c_char_buf = bytes(c_char_buf,'ascii')
                    c_buf_len = 1

                elif param_types[col_num][0] == 'D': #Decimal
                    sign = param_val.as_tuple()[0] == 0 and '+' or '-'
                    digit_string = ''.join([str(x) for x in param_val.as_tuple()[1]])
                    digit_num, dec_num = param_types[col_num][1]
                    if dec_num > 0:
                        # has decimal
                        left_part = digit_string[:digit_num - dec_num]
                        right_part = digit_string[0-dec_num:]
                    else:
                        # no decimal
                        left_part = digit_string + '0'*(0-dec_num)
                        right_part = ''
                    v = ''.join((sign, left_part,'.', right_part))

                    if py_v3:
                        c_char_buf = bytes(v,'ascii')
                    else:
                        c_char_buf = v
                    c_buf_len = len(c_char_buf)

                elif param_types[col_num][0] == 'bi':
                    c_char_buf = str_8b(param_val)
                    c_buf_len = len(c_char_buf)

                else:
                    c_char_buf = param_val


                if param_types[col_num][0] == 'bi':
                    param_buffer.raw = str_8b(param_val)

                else:
                    #print (type(param_val),param_buffer, param_buffer.value)
                    param_buffer.value = c_char_buf

                if param_types[col_num][0] in ('U','u','S','s'):
                    #ODBC driver will find NUL in unicode and string to determine their length
                    param_buffer_len.value = SQL_NTS
                else:
                    param_buffer_len.value = c_buf_len

                col_num += 1
            ret = SQLExecute(self.stmt_h)
            if ret != SQL_SUCCESS:
                #print param_valparam_buffer, param_buffer.value
                check_success(self, ret)


            if not many_mode:
                self._NumOfRows()
                self._UpdateDesc()
                #self._BindCols()

        else:
            self.execdirect(query_string)
        return self


    def _SQLExecute(self):
        if not self.connection:
            self.close()
        ret = SQLExecute(self.stmt_h)
        if ret != SQL_SUCCESS:
            check_success(self, ret)


    def execdirect(self, query_string):
        """Execute a query directly"""
        if not self.connection:
            self.close()

        self._free_stmt()
        self._last_param_types = None
        self.statement = None
        if type(query_string) == unicode:
            c_query_string = wchar_pointer(UCS_buf(query_string))
            ret = ODBC_API.SQLExecDirectW(self.stmt_h, c_query_string, len(query_string))
        else:
            c_query_string = ctypes.c_char_p(query_string)
            ret = ODBC_API.SQLExecDirect(self.stmt_h, c_query_string, len(query_string))
        check_success(self, ret)
        self._NumOfRows()
        self._UpdateDesc()
        #self._BindCols()
        return self


    def callproc(self, procname, args):
        if not self.connection:
            self.close()
        raise Warning('', 'Still not fully implemented')
        self._pram_io_list = [row[4] for row in self.procedurecolumns(procedure = procname).fetchall() if row[4] not in (SQL_RESULT_COL, SQL_RETURN_VALUE)]

        print('pram_io_list: '+str(self._pram_io_list))



        call_escape = '{CALL '+procname
        if args:
            call_escape += '(' + ','.join(['?' for params in args]) + ')'
        call_escape += '}'

        self.execute(call_escape, args, call_mode = True)

        result = []

        for buf, buf_len, sql_type in self._ParamBufferList:
            if buf_len.value == -1:
                result.append(None)
            else:
                result.append(self.connection.output_converter[sql_type](buf.value))
        return result



    def executemany(self, query_string, params_list = [None]):
        if not self.connection:
            self.close()

        for params in params_list:
            self.execute(query_string, params, many_mode = True)
        self._NumOfRows()
        self.rowcount = -1
        self._UpdateDesc()
        #self._BindCols()



    def _CreateColBuf(self):
        if not self.connection:
            self.close()
        self._free_stmt(SQL_UNBIND)
        NOC = self._NumOfCols()
        self._ColBufferList = []
        bind_data = True
        for col_num in range(NOC):
            col_name = self.description[col_num][0]
            col_size = self.description[col_num][2]
            col_sql_data_type = self._ColTypeCodeList[col_num]

            target_type = SQL_data_type_dict[col_sql_data_type][2]
            dynamic_length = SQL_data_type_dict[col_sql_data_type][5]
            # set default size base on the column's sql data type
            total_buf_len = SQL_data_type_dict[col_sql_data_type][4]

            # over-write if there's pre-set size value for "large columns"
            if total_buf_len > 20500:
                total_buf_len = self._outputsize.get(None,total_buf_len)
            # over-write if there's pre-set size value for the "col_num" column
            total_buf_len = self._outputsize.get(col_num, total_buf_len)

            # if the size of the buffer is very long, do not bind
            # because a large buffer decrease performance, and sometimes you only get a NULL value.
            # in that case use sqlgetdata instead.
            if col_size >= 1024:
                dynamic_length = True

            alloc_buffer = SQL_data_type_dict[col_sql_data_type][3](total_buf_len)

            used_buf_len = c_ssize_t()

            force_unicode = self.connection.unicode_results

            if force_unicode and col_sql_data_type in (SQL_CHAR,SQL_VARCHAR,SQL_LONGVARCHAR):
                target_type = SQL_C_WCHAR
                alloc_buffer = create_buffer_u(total_buf_len)

            buf_cvt_func = self.connection.output_converter[self._ColTypeCodeList[col_num]]

            if bind_data:
                if dynamic_length:
                    bind_data = False
            self._ColBufferList.append([col_name, target_type, used_buf_len, ADDR(used_buf_len), alloc_buffer, ADDR(alloc_buffer), total_buf_len, buf_cvt_func, bind_data])

            if bind_data:
                ret = ODBC_API.SQLBindCol(self.stmt_h, col_num + 1, target_type, ADDR(alloc_buffer), total_buf_len, ADDR(used_buf_len))
                if ret != SQL_SUCCESS:
                    check_success(self, ret)

    def _UpdateDesc(self):
        "Get the information of (name, type_code, display_size, internal_size, col_precision, scale, null_ok)"
        if not self.connection:
            self.close()

        force_unicode = self.connection.unicode_results
        if force_unicode:
            Cname = create_buffer_u(1024)
        else:
            Cname = create_buffer(1024)

        Cname_ptr = c_short()
        Ctype_code = c_short()
        Csize = ctypes.c_size_t()
        Cdisp_size = c_ssize_t(0)
        CDecimalDigits = c_short()
        Cnull_ok = c_short()
        ColDescr = []
        self._ColTypeCodeList = []
        NOC = self._NumOfCols()
        for col in range(1, NOC+1):

            ret = ODBC_API.SQLColAttribute(self.stmt_h, col, SQL_DESC_DISPLAY_SIZE, ADDR(create_buffer(10)),
                10, ADDR(c_short()),ADDR(Cdisp_size))
            if ret != SQL_SUCCESS:
                check_success(self, ret)

            if force_unicode:

                ret = ODBC_API.SQLDescribeColW(self.stmt_h, col, Cname, len(Cname), ADDR(Cname_ptr),\
                    ADDR(Ctype_code),ADDR(Csize),ADDR(CDecimalDigits), ADDR(Cnull_ok))
                if ret != SQL_SUCCESS:
                    check_success(self, ret)
            else:

                ret = ODBC_API.SQLDescribeCol(self.stmt_h, col, Cname, len(Cname), ADDR(Cname_ptr),\
                    ADDR(Ctype_code),ADDR(Csize),ADDR(CDecimalDigits), ADDR(Cnull_ok))
                if ret != SQL_SUCCESS:
                    check_success(self, ret)

            col_name = Cname.value
            if lowercase:
                col_name = col_name.lower()
            #(name, type_code, display_size,

            ColDescr.append((col_name, SQL_data_type_dict.get(Ctype_code.value,(Ctype_code.value,))[0],Cdisp_size.value,\
                Csize.value, Csize.value,CDecimalDigits.value,Cnull_ok.value == 1 and True or False))
            self._ColTypeCodeList.append(Ctype_code.value)

        if len(ColDescr) > 0:
            self.description = ColDescr
            # Create the row type before fetching.
            self._row_type = self.row_type_callable(self)
        else:
            self.description = None
        self._CreateColBuf()


    def _NumOfRows(self):
        """Get the number of rows"""
        if not self.connection:
            self.close()

        NOR = c_ssize_t()
        ret = SQLRowCount(self.stmt_h, ADDR(NOR))
        if ret != SQL_SUCCESS:
            check_success(self, ret)
        self.rowcount = NOR.value
        return self.rowcount


    def _NumOfCols(self):
        """Get the number of cols"""
        if not self.connection:
            self.close()

        NOC = c_short()
        ret = SQLNumResultCols(self.stmt_h, ADDR(NOC))
        if ret != SQL_SUCCESS:
            check_success(self, ret)
        return NOC.value


    def fetchall(self):
        if not self.connection:
            self.close()

        rows = []
        while True:
            row = self.fetchone()
            if row is None:
                break
            rows.append(row)
        return rows


    def fetchmany(self, num = None):
        if not self.connection:
            self.close()

        if num is None:
            num = self.arraysize
        rows = []

        while len(rows) < num:
            row = self.fetchone()
            if row is None:
                break
            rows.append(row)
        return rows


    def fetchone(self):
        if not self.connection:
            self.close()

        ret = SQLFetch(self.stmt_h)

        if ret in (SQL_SUCCESS,SQL_SUCCESS_WITH_INFO):
            '''Bind buffers for the record set columns'''

            value_list = []
            col_num = 1
            for col_name, target_type, used_buf_len, ADDR_used_buf_len, alloc_buffer, ADDR_alloc_buffer, total_buf_len, buf_cvt_func, bind_data in self._ColBufferList:
                raw_data_parts = []
                while 1:
                    if bind_data:
                        ret = SQL_SUCCESS
                    else:
                        ret = SQLGetData(self.stmt_h, col_num, target_type, ADDR_alloc_buffer, total_buf_len, ADDR_used_buf_len)
                    if ret == SQL_SUCCESS:
                        if used_buf_len.value == SQL_NULL_DATA:
                            value_list.append(None)
                        else:
                            if raw_data_parts == []:
                                # Means no previous data, no need to combine
                                if target_type == SQL_C_BINARY:
                                    value_list.append(buf_cvt_func(alloc_buffer.raw[:used_buf_len.value]))
                                elif target_type == SQL_C_WCHAR:
                                    value_list.append(buf_cvt_func(from_buffer_u(alloc_buffer)))
                                else:
                                    value_list.append(buf_cvt_func(alloc_buffer.value))
                            else:
                                # There are previous fetched raw data to combine
                                if target_type == SQL_C_BINARY:
                                    raw_data_parts.append(alloc_buffer.raw[:used_buf_len.value])
                                elif target_type == SQL_C_WCHAR:
                                    raw_data_parts.append(from_buffer_u(alloc_buffer))
                                else:
                                    raw_data_parts.append(alloc_buffer.value)
                        break

                    elif ret == SQL_SUCCESS_WITH_INFO:
                        # Means the data is only partial
                        if target_type == SQL_C_BINARY:
                            raw_data_parts.append(alloc_buffer.raw)
                        else:
                            raw_data_parts.append(alloc_buffer.value)

                    elif ret == SQL_NO_DATA:
                        # Means all data has been transmitted
                        break
                    else:
                        check_success(self, ret)

                if raw_data_parts != []:
                    if py_v3:
                        if target_type != SQL_C_BINARY:
                            raw_value = ''.join(raw_data_parts)
                        else:
                            raw_value = BLANK_BYTE.join(raw_data_parts)
                    else:
                        raw_value = ''.join(raw_data_parts)

                    value_list.append(buf_cvt_func(raw_value))
                col_num += 1

            return self._row_type(value_list)

        else:
            if ret == SQL_NO_DATA_FOUND:

                return None
            else:
                check_success(self, ret)

    def __next__(self):
        return self.next()

    def next(self):
        row = self.fetchone()
        if row is None:
            raise(StopIteration)
        return row

    def __iter__(self):
        return self


    def skip(self, count = 0):
        if not self.connection:
            self.close()

        for i in range(count):
            ret = ODBC_API.SQLFetchScroll(self.stmt_h, SQL_FETCH_NEXT, 0)
            if ret != SQL_SUCCESS:
                check_success(self, ret)
        return None



    def nextset(self):
        if not self.connection:
            self.close()

        ret = ODBC_API.SQLMoreResults(self.stmt_h)
        if ret not in (SQL_SUCCESS, SQL_NO_DATA):
            check_success(self, ret)

        if ret == SQL_NO_DATA:
            self._free_stmt()
            return False
        else:
            self._NumOfRows()
            self._UpdateDesc()
            #self._BindCols()
        return True


    def _free_stmt(self, free_type = None):
        if not self.connection:
            self.close()

        if not self.connection.connected:
            raise ProgrammingError('HY000','Attempt to use a closed connection.')

        #self.description = None
        #self.rowcount = -1
        if free_type in (SQL_CLOSE, None):
            ret = ODBC_API.SQLFreeStmt(self.stmt_h, SQL_CLOSE)
            if ret != SQL_SUCCESS:
                check_success(self, ret)
        if free_type in (SQL_UNBIND, None):
            ret = ODBC_API.SQLFreeStmt(self.stmt_h, SQL_UNBIND)
            if ret != SQL_SUCCESS:
                check_success(self, ret)
        if free_type in (SQL_RESET_PARAMS, None):
            ret = ODBC_API.SQLFreeStmt(self.stmt_h, SQL_RESET_PARAMS)
            if ret != SQL_SUCCESS:
                check_success(self, ret)



    def getTypeInfo(self, sqlType = None):
        if not self.connection:
            self.close()

        if sqlType is None:
            type = SQL_ALL_TYPES
        else:
            type = sqlType
        ret = ODBC_API.SQLGetTypeInfo(self.stmt_h, type)
        if ret in (SQL_SUCCESS, SQL_SUCCESS_WITH_INFO):
            self._NumOfRows()
            self._UpdateDesc()
            #self._BindCols()
            return self.fetchone()


    def tables(self, table=None, catalog=None, schema=None, tableType=None):
        """Return a list with all tables"""
        if not self.connection:
            self.close()

        l_catalog = l_schema = l_table = l_tableType = 0

        if unicode in [type(x) for x in (table, catalog, schema,tableType)]:
            string_p = lambda x:wchar_pointer(UCS_buf(x))
            API_f = ODBC_API.SQLTablesW
        else:
            string_p = ctypes.c_char_p
            API_f = ODBC_API.SQLTables



        if catalog is not None:
            l_catalog = len(catalog)
            catalog = string_p(catalog)

        if schema is not None:
            l_schema = len(schema)
            schema = string_p(schema)

        if table is not None:
            l_table = len(table)
            table = string_p(table)

        if tableType is not None:
            l_tableType = len(tableType)
            tableType = string_p(tableType)

        self._free_stmt()
        self._last_param_types = None
        self.statement = None
        ret = API_f(self.stmt_h,
                                catalog, l_catalog,
                                schema, l_schema,
                                table, l_table,
                                tableType, l_tableType)
        check_success(self, ret)

        self._NumOfRows()
        self._UpdateDesc()
        #self._BindCols()
        return self


    def columns(self, table=None, catalog=None, schema=None, column=None):
        """Return a list with all columns"""
        if not self.connection:
            self.close()

        l_catalog = l_schema = l_table = l_column = 0

        if unicode in [type(x) for x in (table, catalog, schema,column)]:
            string_p = lambda x:wchar_pointer(UCS_buf(x))
            API_f = ODBC_API.SQLColumnsW
        else:
            string_p = ctypes.c_char_p
            API_f = ODBC_API.SQLColumns



        if catalog is not None:
            l_catalog = len(catalog)
            catalog = string_p(catalog)
        if schema is not None:
            l_schema = len(schema)
            schema = string_p(schema)
        if table is not None:
            l_table = len(table)
            table = string_p(table)
        if column is not None:
            l_column = len(column)
            column = string_p(column)

        self._free_stmt()
        self._last_param_types = None
        self.statement = None

        ret = API_f(self.stmt_h,
                    catalog, l_catalog,
                    schema, l_schema,
                    table, l_table,
                    column, l_column)
        check_success(self, ret)

        self._NumOfRows()
        self._UpdateDesc()
        #self._BindCols()
        return self


    def primaryKeys(self, table=None, catalog=None, schema=None):
        if not self.connection:
            self.close()

        l_catalog = l_schema = l_table = 0

        if unicode in [type(x) for x in (table, catalog, schema)]:
            string_p = lambda x:wchar_pointer(UCS_buf(x))
            API_f = ODBC_API.SQLPrimaryKeysW
        else:
            string_p = ctypes.c_char_p
            API_f = ODBC_API.SQLPrimaryKeys



        if catalog is not None:
            l_catalog = len(catalog)
            catalog = string_p(catalog)

        if schema is not None:
            l_schema = len(schema)
            schema = string_p(schema)

        if table is not None:
            l_table = len(table)
            table = string_p(table)

        self._free_stmt()
        self._last_param_types = None
        self.statement = None

        ret = API_f(self.stmt_h,
                    catalog, l_catalog,
                    schema, l_schema,
                    table, l_table)
        check_success(self, ret)

        self._NumOfRows()
        self._UpdateDesc()
        #self._BindCols()
        return self


    def foreignKeys(self, table=None, catalog=None, schema=None, foreignTable=None, foreignCatalog=None, foreignSchema=None):
        if not self.connection:
            self.close()

        l_catalog = l_schema = l_table = l_foreignTable = l_foreignCatalog = l_foreignSchema = 0

        if unicode in [type(x) for x in (table, catalog, schema,foreignTable,foreignCatalog,foreignSchema)]:
            string_p = lambda x:wchar_pointer(UCS_buf(x))
            API_f = ODBC_API.SQLForeignKeysW
        else:
            string_p = ctypes.c_char_p
            API_f = ODBC_API.SQLForeignKeys

        if catalog is not None:
            l_catalog = len(catalog)
            catalog = string_p(catalog)
        if schema is not None:
            l_schema = len(schema)
            schema = string_p(schema)
        if table is not None:
            l_table = len(table)
            table = string_p(table)
        if foreignTable is not None:
            l_foreignTable = len(foreignTable)
            foreignTable = string_p(foreignTable)
        if foreignCatalog is not None:
            l_foreignCatalog = len(foreignCatalog)
            foreignCatalog = string_p(foreignCatalog)
        if foreignSchema is not None:
            l_foreignSchema = len(foreignSchema)
            foreignSchema = string_p(foreignSchema)

        self._free_stmt()
        self._last_param_types = None
        self.statement = None

        ret = API_f(self.stmt_h,
                    catalog, l_catalog,
                    schema, l_schema,
                    table, l_table,
                    foreignCatalog, l_foreignCatalog,
                    foreignSchema, l_foreignSchema,
                    foreignTable, l_foreignTable)
        check_success(self, ret)

        self._NumOfRows()
        self._UpdateDesc()
        #self._BindCols()
        return self


    def procedurecolumns(self, procedure=None, catalog=None, schema=None, column=None):
        if not self.connection:
            self.close()

        l_catalog = l_schema = l_procedure = l_column = 0
        if unicode in [type(x) for x in (procedure, catalog, schema,column)]:
            string_p = lambda x:wchar_pointer(UCS_buf(x))
            API_f = ODBC_API.SQLProcedureColumnsW
        else:
            string_p = ctypes.c_char_p
            API_f = ODBC_API.SQLProcedureColumns


        if catalog is not None:
            l_catalog = len(catalog)
            catalog = string_p(catalog)
        if schema is not None:
            l_schema = len(schema)
            schema = string_p(schema)
        if procedure is not None:
            l_procedure = len(procedure)
            procedure = string_p(procedure)
        if column is not None:
            l_column = len(column)
            column = string_p(column)


        self._free_stmt()
        self._last_param_types = None
        self.statement = None

        ret = API_f(self.stmt_h,
                    catalog, l_catalog,
                    schema, l_schema,
                    procedure, l_procedure,
                    column, l_column)
        check_success(self, ret)

        self._NumOfRows()
        self._UpdateDesc()
        return self


    def procedures(self, procedure=None, catalog=None, schema=None):
        if not self.connection:
            self.close()

        l_catalog = l_schema = l_procedure = 0

        if unicode in [type(x) for x in (procedure, catalog, schema)]:
            string_p = lambda x:wchar_pointer(UCS_buf(x))
            API_f = ODBC_API.SQLProceduresW
        else:
            string_p = ctypes.c_char_p
            API_f = ODBC_API.SQLProcedures



        if catalog is not None:
            l_catalog = len(catalog)
            catalog = string_p(catalog)
        if schema is not None:
            l_schema = len(schema)
            schema = string_p(schema)
        if procedure is not None:
            l_procedure = len(procedure)
            procedure = string_p(procedure)


        self._free_stmt()
        self._last_param_types = None
        self.statement = None

        ret = API_f(self.stmt_h,
                    catalog, l_catalog,
                    schema, l_schema,
                    procedure, l_procedure)
        check_success(self, ret)

        self._NumOfRows()
        self._UpdateDesc()
        return self


    def statistics(self, table, catalog=None, schema=None, unique=False, quick=True):
        if not self.connection:
            self.close()

        l_table = l_catalog = l_schema = 0

        if unicode in [type(x) for x in (table, catalog, schema)]:
            string_p = lambda x:wchar_pointer(UCS_buf(x))
            API_f = ODBC_API.SQLStatisticsW
        else:
            string_p = ctypes.c_char_p
            API_f = ODBC_API.SQLStatistics


        if catalog is not None:
            l_catalog = len(catalog)
            catalog = string_p(catalog)
        if schema is not None:
            l_schema = len(schema)
            schema = string_p(schema)
        if table is not None:
            l_table = len(table)
            table = string_p(table)

        if unique:
            Unique = SQL_INDEX_UNIQUE
        else:
            Unique = SQL_INDEX_ALL
        if quick:
            Reserved = SQL_QUICK
        else:
            Reserved = SQL_ENSURE

        self._free_stmt()
        self._last_param_types = None
        self.statement = None

        ret = API_f(self.stmt_h,
                    catalog, l_catalog,
                    schema, l_schema,
                    table, l_table,
                    Unique, Reserved)
        check_success(self, ret)

        self._NumOfRows()
        self._UpdateDesc()
        #self._BindCols()
        return self


    def commit(self):
        if not self.connection:
            self.close()
        self.connection.commit()

    def rollback(self):
        if not self.connection:
            self.close()
        self.connection.rollback()

    def setoutputsize(self, size, column = None):
        if not self.connection:
            self.close()
        self._outputsize[column] = size

    def setinputsizes(self, sizes):
        if not self.connection:
            self.close()
        self._inputsizers = [size for size in sizes]


    def close(self):
        """ Call SQLCloseCursor API to free the statement handle"""
#        ret = ODBC_API.SQLCloseCursor(self.stmt_h)
#        check_success(self, ret)
#
        if self.connection.connected:
            ret = ODBC_API.SQLFreeStmt(self.stmt_h, SQL_CLOSE)
            check_success(self, ret)

            ret = ODBC_API.SQLFreeStmt(self.stmt_h, SQL_UNBIND)
            check_success(self, ret)

            ret = ODBC_API.SQLFreeStmt(self.stmt_h, SQL_RESET_PARAMS)
            check_success(self, ret)

            ret = ODBC_API.SQLFreeHandle(SQL_HANDLE_STMT, self.stmt_h)
            check_success(self, ret)


        self.closed = True


    def __del__(self):
        if not self.closed:
            self.close()

    def __exit__(self, type, value, traceback):
        if not self.connection:
            self.close()

        if value:
            self.rollback()
        else:
            self.commit()

        self.close()


    def __enter__(self):
        return self


# This class implement a odbc connection.
#
#


class Connection:
    def __init__(self, connectString = '', autocommit = False, ansi = False, timeout = 0, unicode_results = use_unicode, readonly = False, **kargs):
        """Init variables and connect to the engine"""
        self.connected = 0
        self.type_size_dic = {}
        self.ansi = False
        self.unicode_results = False
        self.dbc_h = ctypes.c_void_p()
        self.autocommit = autocommit
        self.readonly = False
        self.timeout = 0
        # self._cursors = []
        for key, value in list(kargs.items()):
            connectString = connectString + key + '=' + value + ';'
        self.connectString = connectString


        self.clear_output_converters()

        try:
            lock.acquire()
            if shared_env_h is None:
                #Initialize an enviroment if it is not created.
                AllocateEnv()
        finally:
            lock.release()

        # Allocate an DBC handle self.dbc_h under the environment shared_env_h
        # This DBC handle is actually the basis of a "connection"
        # The handle of self.dbc_h will be used to connect to a certain source
        # in the self.connect and self.ConnectByDSN method

        ret = ODBC_API.SQLAllocHandle(SQL_HANDLE_DBC, shared_env_h, ADDR(self.dbc_h))
        check_success(self, ret)

        self.connect(connectString, autocommit, ansi, timeout, unicode_results, readonly)



    def connect(self, connectString = '', autocommit = False, ansi = False, timeout = 0, unicode_results = use_unicode, readonly = False):
        """Connect to odbc, using connect strings and set the connection's attributes like autocommit and timeout
        by calling SQLSetConnectAttr
        """

        # Before we establish the connection by the connection string
        # Set the connection's attribute of "timeout" (Actully LOGIN_TIMEOUT)
        if timeout != 0:
            self.settimeout(timeout)
            ret = ODBC_API.SQLSetConnectAttr(self.dbc_h, SQL_ATTR_LOGIN_TIMEOUT, timeout, SQL_IS_UINTEGER);
            check_success(self, ret)


        # Create one connection with a connect string by calling SQLDriverConnect
        # and make self.dbc_h the handle of this connection


        # Convert the connetsytring to encoded string
        # so it can be converted to a ctypes c_char array object


        self.ansi = ansi
        if not ansi:
            c_connectString = wchar_pointer(UCS_buf(self.connectString))
            odbc_func = ODBC_API.SQLDriverConnectW
        else:
            c_connectString = ctypes.c_char_p(self.connectString)
            odbc_func = ODBC_API.SQLDriverConnect

        # With unixODBC, SQLDriverConnect will intermittently fail with error:
        #    [01000] [unixODBC][Driver Manager]Can't open lib '/path/to/so' : file not found"
        # or:
        #    [01000] [unixODBC][Driver Manager]Can't open lib '/path/to/so' : (null)"
        # when called concurrently by more than one threads. So, we have to
        # use a lock to serialize the calls. By the way, the error is much
        # less likely to happen if ODBC Tracing is enabled, likely due to the
        # implicit serialization caused by writing to trace file.
        if ODBC_API._name != 'odbc32':
            try:
                lock.acquire()
                ret = odbc_func(self.dbc_h, 0, c_connectString, len(self.connectString), None, 0, None, SQL_DRIVER_NOPROMPT)
            finally:
                lock.release()
        else:
            ret = odbc_func(self.dbc_h, 0, c_connectString, len(self.connectString), None, 0, None, SQL_DRIVER_NOPROMPT)
        check_success(self, ret)


        # Set the connection's attribute of "autocommit"
        #
        self.autocommit = autocommit

        if self.autocommit == True:
            ret = ODBC_API.SQLSetConnectAttr(self.dbc_h, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON, SQL_IS_UINTEGER)
        else:
            ret = ODBC_API.SQLSetConnectAttr(self.dbc_h, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF, SQL_IS_UINTEGER)
        check_success(self, ret)

        # Set the connection's attribute of "readonly"
        #
        self.readonly = readonly

        ret = ODBC_API.SQLSetConnectAttr(self.dbc_h, SQL_ATTR_ACCESS_MODE, self.readonly and SQL_MODE_READ_ONLY or SQL_MODE_READ_WRITE, SQL_IS_UINTEGER)
        check_success(self, ret)

        self.unicode_results = unicode_results
        self.connected = 1
        self.update_db_special_info()

    def clear_output_converters(self):
        self.output_converter = {}
        for sqltype, profile in SQL_data_type_dict.items():
            self.output_converter[sqltype] = profile[1]


    def add_output_converter(self, sqltype, func):
        self.output_converter[sqltype] = func

    def settimeout(self, timeout):
        ret = ODBC_API.SQLSetConnectAttr(self.dbc_h, SQL_ATTR_CONNECTION_TIMEOUT, timeout, SQL_IS_UINTEGER);
        check_success(self, ret)
        self.timeout = timeout


    def ConnectByDSN(self, dsn, user, passwd = ''):
        """Connect to odbc, we need dsn, user and optionally password"""
        self.dsn = dsn
        self.user = user
        self.passwd = passwd

        sn = create_buffer(dsn)
        un = create_buffer(user)
        pw = create_buffer(passwd)

        ret = ODBC_API.SQLConnect(self.dbc_h, sn, len(sn), un, len(un), pw, len(pw))
        check_success(self, ret)

        self.update_db_special_info()
        self.connected = 1


    def cursor(self, row_type_callable=None):
        #self.settimeout(self.timeout)
        if not self.connected:
            raise ProgrammingError('HY000','Attempt to use a closed connection.')
        cur = Cursor(self, row_type_callable=row_type_callable)
        # self._cursors.append(cur)
        return cur

    def update_db_special_info(self):
        for sql_type in (
            SQL_TYPE_TIMESTAMP,
            SQL_TYPE_DATE,
            SQL_TYPE_TIME,
            SQL_SS_TIME2,
        ):
            cur = Cursor(self)

            try:
                info_tuple = cur.getTypeInfo(sql_type)
                if info_tuple is not None:
                    self.type_size_dic[sql_type] = info_tuple[2], info_tuple[14]
            except:
                pass
            cur.close()

        self.support_SQLDescribeParam = False
        try:
            driver_name = self.getinfo(SQL_DRIVER_NAME)
            if any(x in driver_name for x in ('SQLSRV','ncli','libsqlncli')):
                self.support_SQLDescribeParam = True
        except:
            pass

    def commit(self):
        if not self.connected:
            raise ProgrammingError('HY000','Attempt to use a closed connection.')

        ret = SQLEndTran(SQL_HANDLE_DBC, self.dbc_h, SQL_COMMIT)
        if ret != SQL_SUCCESS:
            check_success(self, ret)

    def rollback(self):
        if not self.connected:
            raise ProgrammingError('HY000','Attempt to use a closed connection.')
        ret = SQLEndTran(SQL_HANDLE_DBC, self.dbc_h, SQL_ROLLBACK)
        if ret != SQL_SUCCESS:
            check_success(self, ret)



    def getinfo(self,infotype):
        if infotype not in list(aInfoTypes.keys()):
            raise ProgrammingError('HY000','Invalid getinfo value: '+str(infotype))


        if aInfoTypes[infotype] == 'GI_UINTEGER':
            total_buf_len = 1000
            alloc_buffer = ctypes.c_ulong()
            used_buf_len = c_short()
            ret = ODBC_API.SQLGetInfo(self.dbc_h,infotype,ADDR(alloc_buffer), total_buf_len,\
                    ADDR(used_buf_len))
            check_success(self, ret)
            result = alloc_buffer.value

        elif aInfoTypes[infotype] == 'GI_USMALLINT':
            total_buf_len = 1000
            alloc_buffer = ctypes.c_ushort()
            used_buf_len = c_short()
            ret = ODBC_API.SQLGetInfo(self.dbc_h,infotype,ADDR(alloc_buffer), total_buf_len,\
                    ADDR(used_buf_len))
            check_success(self, ret)
            result = alloc_buffer.value

        else:
            total_buf_len = 1000
            alloc_buffer = create_buffer(total_buf_len)
            used_buf_len = c_short()
            if self.ansi:
                API_f = ODBC_API.SQLGetInfo
            else:
                API_f = ODBC_API.SQLGetInfoW
            ret = API_f(self.dbc_h,infotype,ADDR(alloc_buffer), total_buf_len,\
                    ADDR(used_buf_len))
            check_success(self, ret)
            if self.ansi:
                result = alloc_buffer.value
            else:
                result = UCS_dec(alloc_buffer)
            if aInfoTypes[infotype] == 'GI_YESNO':
                if unicode(result[0]) == unicode('Y'):
                    result = True
                else:
                    result = False

        return result

    def __exit__(self, type, value, traceback):
        if value:
            self.rollback()
        else:
            self.commit()

        if self.connected:
            self.close()

    def __enter__(self):
        return self

    def __del__(self):
        if self.connected:
            self.close()

    def close(self):
        if not self.connected:
            raise ProgrammingError('HY000','Attempt to close a closed connection.')
        # for cur in self._cursors:
            # if not cur is None:
                # if not cur.closed:
                    # cur.close()

        if self.connected:
            #if DEBUG:print 'disconnect'
            if not self.autocommit:
                self.rollback()
            ret = ODBC_API.SQLDisconnect(self.dbc_h)
            check_success(self, ret)
        #if DEBUG:print 'free dbc'
        ret = ODBC_API.SQLFreeHandle(SQL_HANDLE_DBC, self.dbc_h)
        check_success(self, ret)
#        if shared_env_h.value:
#            #if DEBUG:print 'env'
#            ret = ODBC_API.SQLFreeHandle(SQL_HANDLE_ENV, shared_env_h)
#            check_success(shared_env_h, ret)
        self.connected = 0

odbc = Connection
connect = odbc
'''
def connect(connectString = '', autocommit = False, ansi = False, timeout = 0, unicode_results = False, readonly = False, **kargs):
    return odbc(connectString, autocommit, ansi, timeout, unicode_results, readonly, kargs)
'''

def drivers():
    if sys.platform not in ('win32','cli'):
        raise Exception('This function is available for use in Windows only.')
    try:
        lock.acquire()
        if shared_env_h is None:
            AllocateEnv()
    finally:
        lock.release()

    DriverDescription = create_buffer_u(1000)
    BufferLength1 = c_short(1000)
    DescriptionLength = c_short()
    DriverAttributes = create_buffer_u(1000)
    BufferLength2 = c_short(1000)
    AttributesLength = c_short()
    ret = SQL_SUCCESS
    DriverList = []
    Direction = SQL_FETCH_FIRST
    while ret != SQL_NO_DATA:
        ret = ODBC_API.SQLDriversW(shared_env_h, Direction , DriverDescription , BufferLength1
                    , ADDR(DescriptionLength), DriverAttributes, BufferLength2, ADDR(AttributesLength))
        check_success(shared_env_h, ret)
        DriverList.append(DriverDescription.value)
        if Direction == SQL_FETCH_FIRST:
            Direction = SQL_FETCH_NEXT
    return DriverList




def win_create_mdb(mdb_path, sort_order = "General\0\0"):
    if sys.platform not in ('win32','cli'):
        raise Exception('This function is available for use in Windows only.')

    mdb_driver = [d for d in drivers() if 'Microsoft Access Driver (*.mdb' in d]
    if mdb_driver == []:
        raise Exception('Access Driver is not found.')
    else:
        driver_name = mdb_driver[0].encode('mbcs')


    #CREATE_DB=<path name> <sort order>
    ctypes.windll.ODBCCP32.SQLConfigDataSource.argtypes = [ctypes.c_void_p,ctypes.c_ushort,ctypes.c_char_p,ctypes.c_char_p]

    if py_v3:
        c_Path =  bytes("CREATE_DB=" + mdb_path + " " + sort_order,'mbcs')
    else:
        c_Path =  "CREATE_DB=" + mdb_path + " " + sort_order
    ODBC_ADD_SYS_DSN = 1


    ret = ctypes.windll.ODBCCP32.SQLConfigDataSource(None,ODBC_ADD_SYS_DSN,driver_name, c_Path)
    if not ret:
        raise Exception('Failed to create Access mdb file - "%s". Please check file path, permission and Access driver readiness.' %mdb_path)


def win_connect_mdb(mdb_path):
    if sys.platform not in ('win32','cli'):
        raise Exception('This function is available for use in Windows only.')

    mdb_driver = [d for d in drivers() if 'Microsoft Access Driver (*.mdb' in d]
    if mdb_driver == []:
        raise Exception('Access Driver is not found.')
    else:
        driver_name = mdb_driver[0]

    return connect('Driver={'+driver_name+"};DBQ="+mdb_path, unicode_results = use_unicode, readonly = False)



def win_compact_mdb(mdb_path, compacted_mdb_path, sort_order = "General\0\0"):
    if sys.platform not in ('win32','cli'):
        raise Exception('This function is available for use in Windows only.')


    mdb_driver = [d for d in drivers() if 'Microsoft Access Driver (*.mdb' in d]
    if mdb_driver == []:
        raise Exception('Access Driver is not found.')
    else:
        driver_name = mdb_driver[0].encode('mbcs')

    #COMPACT_DB=<source path> <destination path> <sort order>
    ctypes.windll.ODBCCP32.SQLConfigDataSource.argtypes = [ctypes.c_void_p,ctypes.c_ushort,ctypes.c_char_p,ctypes.c_char_p]
    #driver_name = "Microsoft Access Driver (*.mdb)"
    if py_v3:
        c_Path = bytes("COMPACT_DB=" + mdb_path + " " + compacted_mdb_path + " " + sort_order,'mbcs')
        #driver_name = bytes(driver_name,'mbcs')
    else:
        c_Path = "COMPACT_DB=" + mdb_path + " " + compacted_mdb_path + " " + sort_order

    ODBC_ADD_SYS_DSN = 1
    ret = ctypes.windll.ODBCCP32.SQLConfigDataSource(None,ODBC_ADD_SYS_DSN,driver_name, c_Path)
    if not ret:
        raise Exception('Failed to compact Access mdb file - "%s". Please check file path, permission and Access driver readiness.' %compacted_mdb_path)


def dataSources():
    """Return a list with [name, descrition]"""
    dsn = create_buffer(1024)
    desc = create_buffer(1024)
    dsn_len = c_short()
    desc_len = c_short()
    dsn_list = {}
    try:
        lock.acquire()
        if shared_env_h is None:
            AllocateEnv()
    finally:
        lock.release()
    while 1:
        ret = ODBC_API.SQLDataSources(shared_env_h, SQL_FETCH_NEXT, \
            dsn, len(dsn), ADDR(dsn_len), desc, len(desc), ADDR(desc_len))
        if ret == SQL_NO_DATA_FOUND:
            break
        elif not ret in (SQL_SUCCESS, SQL_SUCCESS_WITH_INFO):
            ctrl_err(SQL_HANDLE_ENV, shared_env_h, ret)
        else:
            dsn_list[dsn.value] = desc.value
    return dsn_list


def monkey_patch_for_gevent():
    import functools, gevent
    apply_e = gevent.get_hub().threadpool.apply_e
    def monkey_patch(func):
        @functools.wraps(func)
        def wrap(*args, **kwargs):
            #if DEBUG:print('%s called with %s %s' % (func, args, kwargs))
            return apply_e(Exception, func, args, kwargs)
        return wrap
    for attr in dir(ODBC_API):
        if attr.startswith('SQL') and hasattr(getattr(ODBC_API, attr), 'argtypes'):
            setattr(ODBC_API, attr, monkey_patch(getattr(ODBC_API, attr)))
