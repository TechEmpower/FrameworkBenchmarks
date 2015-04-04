# Copyright (c) 2007-2009, Mathieu Fenniak
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright notice,
# this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
# * The name of the author may not be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

__author__ = "Mathieu Fenniak"


exec("from struct import Struct")
for fmt in (
        "i", "h", "q", "d", "f", "iii", "ii", "qii", "dii", "ihihih", "ci",
        "bh", "cccc"):
    exec(fmt + "_struct = Struct('!" + fmt + "')")
    exec(fmt + "_unpack = " + fmt + "_struct.unpack_from")
    exec(fmt + "_pack = " + fmt + "_struct.pack")

import datetime
import time
from .six import binary_type, integer_types, PY2

min_int2, max_int2 = -2 ** 15, 2 ** 15
min_int4, max_int4 = -2 ** 31, 2 ** 31
min_int8, max_int8 = -2 ** 63, 2 ** 63


class Warning(Exception):
    """Generic exception raised for important database warnings like data
    truncations.  This exception is not currently used by pg8000.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class Error(Exception):
    """Generic exception that is the base exception of all other error
    exceptions.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class InterfaceError(Error):
    """Generic exception raised for errors that are related to the database
    interface rather than the database itself.  For example, if the interface
    attempts to use an SSL connection but the server refuses, an InterfaceError
    will be raised.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class DatabaseError(Error):
    """Generic exception raised for errors that are related to the database.
    This exception is currently never raised by pg8000.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class DataError(DatabaseError):
    """Generic exception raised for errors that are due to problems with the
    processed data.  This exception is not currently raised by pg8000.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class OperationalError(DatabaseError):
    """
    Generic exception raised for errors that are related to the database's
    operation and not necessarily under the control of the programmer. This
    exception is currently never raised by pg8000.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class IntegrityError(DatabaseError):
    """
    Generic exception raised when the relational integrity of the database is
    affected.  This exception is not currently raised by pg8000.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class InternalError(DatabaseError):
    """Generic exception raised when the database encounters an internal error.
    This is currently only raised when unexpected state occurs in the pg8000
    interface itself, and is typically the result of a interface bug.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class ProgrammingError(DatabaseError):
    """Generic exception raised for programming errors.  For example, this
    exception is raised if more parameter fields are in a query string than
    there are available parameters.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class NotSupportedError(DatabaseError):
    """Generic exception raised in case a method or database API was used which
    is not supported by the database.

    This exception is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.
    """
    pass


class ArrayContentNotSupportedError(NotSupportedError):
    """
    Raised when attempting to transmit an array where the base type is not
    supported for binary data transfer by the interface.
    """
    pass


class ArrayContentNotHomogenousError(ProgrammingError):
    """
    Raised when attempting to transmit an array that doesn't contain only a
    single type of object.
    """
    pass


class ArrayContentEmptyError(ProgrammingError):
    """Raised when attempting to transmit an empty array. The type oid of an
    empty array cannot be determined, and so sending them is not permitted.
    """
    pass


class ArrayDimensionsNotConsistentError(ProgrammingError):
    """
    Raised when attempting to transmit an array that has inconsistent
    multi-dimension sizes.
    """
    pass


class Bytea(binary_type):
    """Bytea is a str-derived class that is mapped to a PostgreSQL byte array.
    This class is only used in Python 2, the built-in ``bytes`` type is used in
    Python 3.
    """
    pass


class Interval(object):
    """An Interval represents a measurement of time.  In PostgreSQL, an interval
    is defined in the measure of months, days, and microseconds; as such, the
    pg8000 interval type represents the same information.

    Note that values of the :attr:`microseconds`, :attr:`days` and
    :attr:`months` properties are independently measured and cannot be
    converted to each other.  A month may be 28, 29, 30, or 31 days, and a day
    may occasionally be lengthened slightly by a leap second.

    .. attribute:: microseconds

        Measure of microseconds in the interval.

        The microseconds value is constrained to fit into a signed 64-bit
        integer.  Any attempt to set a value too large or too small will result
        in an OverflowError being raised.

    .. attribute:: days

        Measure of days in the interval.

        The days value is constrained to fit into a signed 32-bit integer.
        Any attempt to set a value too large or too small will result in an
        OverflowError being raised.

    .. attribute:: months

        Measure of months in the interval.

        The months value is constrained to fit into a signed 32-bit integer.
        Any attempt to set a value too large or too small will result in an
        OverflowError being raised.
    """

    def __init__(self, microseconds=0, days=0, months=0):
        self.microseconds = microseconds
        self.days = days
        self.months = months

    def _setMicroseconds(self, value):
        if not isinstance(value, integer_types):
            raise TypeError("microseconds must be an integer type")
        elif not (min_int8 < value < max_int8):
            raise OverflowError(
                "microseconds must be representable as a 64-bit integer")
        else:
            self._microseconds = value

    def _setDays(self, value):
        if not isinstance(value, integer_types):
            raise TypeError("days must be an integer type")
        elif not (min_int4 < value < max_int4):
            raise OverflowError(
                "days must be representable as a 32-bit integer")
        else:
            self._days = value

    def _setMonths(self, value):
        if not isinstance(value, integer_types):
            raise TypeError("months must be an integer type")
        elif not (min_int4 < value < max_int4):
            raise OverflowError(
                "months must be representable as a 32-bit integer")
        else:
            self._months = value

    microseconds = property(lambda self: self._microseconds, _setMicroseconds)
    days = property(lambda self: self._days, _setDays)
    months = property(lambda self: self._months, _setMonths)

    def __repr__(self):
        return "<Interval %s months %s days %s microseconds>" % (
            self.months, self.days, self.microseconds)

    def __eq__(self, other):
        return other is not None and isinstance(other, Interval) and \
            self.months == other.months and self.days == other.days and \
            self.microseconds == other.microseconds

    def __neq__(self, other):
        return not self.__eq__(other)

from .core import Connection


def connect(
        user=None, host='localhost', unix_sock=None, port=5432, database=None,
        password=None, ssl=False, **kwargs):
    """Creates a connection to a PostgreSQL database.

    This function is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_; however, the arguments of the
    function are not defined by the specification.

    :param user:
        The username to connect to the PostgreSQL server with. If this is not
        provided, pg8000 looks first for the PGUSER then the USER environment
        variables.

        If your server character encoding is not ``ascii`` or ``utf8``, then
        you need to provide ``user`` as bytes, eg.
        ``"my_name".encode('EUC-JP')``.

    :keyword host:
        The hostname of the PostgreSQL server to connect with.  Providing this
        parameter is necessary for TCP/IP connections.  One of either ``host``
        or ``unix_sock`` must be provided. The default is ``localhost``.

    :keyword unix_sock:
        The path to the UNIX socket to access the database through, for
        example, ``'/tmp/.s.PGSQL.5432'``.  One of either ``host`` or
        ``unix_sock`` must be provided.

    :keyword port:
        The TCP/IP port of the PostgreSQL server instance.  This parameter
        defaults to ``5432``, the registered common port of PostgreSQL TCP/IP
        servers.

    :keyword database:
        The name of the database instance to connect with.  This parameter is
        optional; if omitted, the PostgreSQL server will assume the database
        name is the same as the username.

        If your server character encoding is not ``ascii`` or ``utf8``, then
        you need to provide ``database`` as bytes, eg.
        ``"my_db".encode('EUC-JP')``.

    :keyword password:
        The user password to connect to the server with.  This parameter is
        optional; if omitted and the database server requests password-based
        authentication, the connection will fail to open.  If this parameter
        is provided but not requested by the server, no error will occur.

    :keyword ssl:
        Use SSL encryption for TCP/IP sockets if ``True``.  Defaults to
        ``False``.

    :rtype:
        A :class:`Connection` object.
    """
    return Connection(
        user, host, unix_sock, port, database, password, ssl)

apilevel = "2.0"
"""The DBAPI level supported, currently "2.0".

This property is part of the `DBAPI 2.0 specification
<http://www.python.org/dev/peps/pep-0249/>`_.
"""

threadsafety = 3
"""Integer constant stating the level of thread safety the DBAPI interface
supports.  This DBAPI module supports sharing the module, connections, and
cursors, resulting in a threadsafety value of 3.

This property is part of the `DBAPI 2.0 specification
<http://www.python.org/dev/peps/pep-0249/>`_.
"""

paramstyle = 'format'
"""String property stating the type of parameter marker formatting expected by
the interface.  This value defaults to "format", in which parameters are
marked in this format: "WHERE name=%s".

This property is part of the `DBAPI 2.0 specification
<http://www.python.org/dev/peps/pep-0249/>`_.

As an extension to the DBAPI specification, this value is not constant; it
can be changed to any of the following values:

    qmark
        Question mark style, eg. ``WHERE name=?``
    numeric
        Numeric positional style, eg. ``WHERE name=:1``
    named
        Named style, eg. ``WHERE name=:paramname``
    format
        printf format codes, eg. ``WHERE name=%s``
    pyformat
        Python format codes, eg. ``WHERE name=%(paramname)s``
"""

# I have no idea what this would be used for by a client app.  Should it be
# TEXT, VARCHAR, CHAR?  It will only compare against row_description's
# type_code if it is this one type.  It is the varchar type oid for now, this
# appears to match expectations in the DB API 2.0 compliance test suite.

STRING = 1043
"""String type oid."""

if PY2:
    BINARY = Bytea
else:
    BINARY = bytes

NUMBER = 1700
"""Numeric type oid"""

DATETIME = 1114
"""Timestamp type oid"""

ROWID = 26
"""ROWID type oid"""


def Date(year, month, day):
    """Constuct an object holding a date value.

    This function is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.

    :rtype: :class:`datetime.date`
    """
    return datetime.date(year, month, day)


def Time(hour, minute, second):
    """Construct an object holding a time value.

    This function is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.

    :rtype: :class:`datetime.time`
    """
    return datetime.time(hour, minute, second)


def Timestamp(year, month, day, hour, minute, second):
    """Construct an object holding a timestamp value.

    This function is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.

    :rtype: :class:`datetime.datetime`
    """
    return datetime.datetime(year, month, day, hour, minute, second)


def DateFromTicks(ticks):
    """Construct an object holding a date value from the given ticks value
    (number of seconds since the epoch).

    This function is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.

    :rtype: :class:`datetime.date`
    """
    return Date(*time.localtime(ticks)[:3])


def TimeFromTicks(ticks):
    """Construct an objet holding a time value from the given ticks value
    (number of seconds since the epoch).

    This function is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.

    :rtype: :class:`datetime.time`
    """
    return Time(*time.localtime(ticks)[3:6])


def TimestampFromTicks(ticks):
    """Construct an object holding a timestamp value from the given ticks value
    (number of seconds since the epoch).

    This function is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.

    :rtype: :class:`datetime.datetime`
    """
    return Timestamp(*time.localtime(ticks)[:6])


def Binary(value):
    """Construct an object holding binary data.

    This function is part of the `DBAPI 2.0 specification
    <http://www.python.org/dev/peps/pep-0249/>`_.

    :rtype: :class:`pg8000.types.Bytea` for Python 2, otherwise :class:`bytes`
    """
    if PY2:
        return Bytea(value)
    else:
        return value


from .core import utc, Cursor

__all__ = [
    Warning, Bytea, DataError, DatabaseError, connect, InterfaceError,
    ProgrammingError, Error, OperationalError, IntegrityError, InternalError,
    NotSupportedError, ArrayContentNotHomogenousError, ArrayContentEmptyError,
    ArrayDimensionsNotConsistentError, ArrayContentNotSupportedError, utc,
    Connection, Cursor]

"""Version string for pg8000.

    .. versionadded:: 1.9.11
"""

from ._version import get_versions
__version__ = get_versions()['version']
del get_versions
