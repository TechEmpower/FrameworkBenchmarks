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

import datetime
from datetime import timedelta
from . import (
    Interval, min_int2, max_int2, min_int4, max_int4, min_int8, max_int8,
    Bytea, NotSupportedError, ProgrammingError, InternalError, IntegrityError,
    OperationalError, DatabaseError, InterfaceError, Error,
    ArrayContentNotHomogenousError, ArrayContentEmptyError,
    ArrayDimensionsNotConsistentError, ArrayContentNotSupportedError, Warning,
    i_unpack, ii_unpack, iii_unpack, h_pack, d_unpack, q_unpack, d_pack,
    f_unpack, q_pack, i_pack, h_unpack, dii_unpack, qii_unpack, ci_unpack,
    bh_unpack, ihihih_unpack, cccc_unpack, ii_pack, iii_pack, dii_pack,
    qii_pack)
from warnings import warn
import socket
import threading
from struct import pack
from hashlib import md5
from decimal import Decimal
from collections import deque, defaultdict
from itertools import count, islice
from .six.moves import map
from .six import b, PY2, integer_types, next, PRE_26, text_type, u
from sys import exc_info
from uuid import UUID
from copy import deepcopy
from calendar import timegm
import os
from distutils.version import LooseVersion

try:
    from json import loads
except ImportError:
    pass  # Can only use JSON with Python 2.6 and above


ZERO = timedelta(0)


class UTC(datetime.tzinfo):

    def utcoffset(self, dt):
        return ZERO

    def tzname(self, dt):
        return "UTC"

    def dst(self, dt):
        return ZERO

utc = UTC()

if PRE_26:
    bytearray = list


FC_TEXT = 0
FC_BINARY = 1

BINARY_SPACE = b(" ")
DDL_COMMANDS = b("ALTER"), b("CREATE")


def convert_paramstyle(style, query):
    # I don't see any way to avoid scanning the query string char by char,
    # so we might as well take that careful approach and create a
    # state-based scanner.  We'll use int variables for the state.
    #  0 -- outside quoted string
    #  1 -- inside single-quote string '...'
    #  2 -- inside quoted identifier   "..."
    #  3 -- inside escaped single-quote string, E'...'
    #  4 -- inside parameter name eg. :name
    OUTSIDE = 0
    INSIDE_SQ = 1
    INSIDE_QI = 2
    INSIDE_ES = 3
    INSIDE_PN = 4

    in_quote_escape = False
    in_param_escape = False
    placeholders = []
    output_query = []
    param_idx = map(lambda x: "$" + str(x), count(1))
    state = OUTSIDE
    prev_c = None
    for i, c in enumerate(query):
        if i + 1 < len(query):
            next_c = query[i + 1]
        else:
            next_c = None

        if state == OUTSIDE:
            if c == "'":
                output_query.append(c)
                if prev_c == 'E':
                    state = INSIDE_ES
                else:
                    state = INSIDE_SQ
            elif c == '"':
                output_query.append(c)
                state = INSIDE_QI
            elif style == "qmark" and c == "?":
                output_query.append(next(param_idx))
            elif style == "numeric" and c == ":":
                output_query.append("$")
            elif style == "named" and c == ":":
                state = INSIDE_PN
                placeholders.append('')
            elif style == "pyformat" and c == '%' and next_c == "(":
                state = INSIDE_PN
                placeholders.append('')
            elif style in ("format", "pyformat") and c == "%":
                style = "format"
                if in_param_escape:
                    in_param_escape = False
                    output_query.append(c)
                else:
                    if next_c == "%":
                        in_param_escape = True
                    elif next_c == "s":
                        state = INSIDE_PN
                        output_query.append(next(param_idx))
                    else:
                        raise InterfaceError(
                            "Only %s and %% are supported in the query.")
            else:
                output_query.append(c)

        elif state == INSIDE_SQ:
            if c == "'":
                output_query.append(c)
                if in_quote_escape:
                    in_quote_escape = False
                else:
                    if next_c == "'":
                        in_quote_escape = True
                    else:
                        state = OUTSIDE
            elif style in ("pyformat", "format") and c == "%":
                # hm... we're only going to support an escaped percent sign
                if in_param_escape:
                    in_param_escape = False
                    output_query.append(c)
                else:
                    if next_c == "%":
                        in_param_escape = True
                    else:
                        raise InterfaceError(
                            "'%" + next_c + "' not supported in a quoted "
                            "string within the query string")
            else:
                output_query.append(c)

        elif state == INSIDE_QI:
            if c == '"':
                state = OUTSIDE
                output_query.append(c)
            elif style in ("pyformat", "format") and c == "%":
                # hm... we're only going to support an escaped percent sign
                if in_param_escape:
                    in_param_escape = False
                    output_query.append(c)
                else:
                    if next_c == "%":
                        in_param_escape = True
                    else:
                        raise InterfaceError(
                            "'%" + next_c + "' not supported in a quoted "
                            "string within the query string")
            else:
                output_query.append(c)

        elif state == INSIDE_ES:
            if c == "'" and prev_c != "\\":
                # check for escaped single-quote
                output_query.append(c)
                state = OUTSIDE
            elif style in ("pyformat", "format") and c == "%":
                # hm... we're only going to support an escaped percent sign
                if in_param_escape:
                    in_param_escape = False
                    output_query.append(c)
                else:
                    if next_c == "%":
                        in_param_escape = True
                    else:
                        raise InterfaceError(
                            "'%" + next_c + "' not supported in a quoted "
                            "string within the query string.")
            else:
                output_query.append(c)

        elif state == INSIDE_PN:
            if style == 'named':
                placeholders[-1] += c
                if next_c is None or (not next_c.isalnum() and next_c != '_'):
                    state = OUTSIDE
                    try:
                        pidx = placeholders.index(placeholders[-1], 0, -1)
                        output_query.append("$" + str(pidx + 1))
                        del placeholders[-1]
                    except ValueError:
                        output_query.append("$" + str(len(placeholders)))
            elif style == 'pyformat':
                if prev_c == ')' and c == "s":
                    state = OUTSIDE
                    try:
                        pidx = placeholders.index(placeholders[-1], 0, -1)
                        output_query.append("$" + str(pidx + 1))
                        del placeholders[-1]
                    except ValueError:
                        output_query.append("$" + str(len(placeholders)))
                elif c in "()":
                    pass
                else:
                    placeholders[-1] += c
            elif style == 'format':
                state = OUTSIDE

        prev_c = c

    if style in ('numeric', 'qmark', 'format'):
        def make_args(vals):
            return vals
    else:
        def make_args(vals):
            return tuple(vals[p] for p in placeholders)

    return ''.join(output_query), make_args


EPOCH = datetime.datetime(2000, 1, 1)
EPOCH_TZ = EPOCH.replace(tzinfo=utc)
EPOCH_SECONDS = timegm(EPOCH.timetuple())
utcfromtimestamp = datetime.datetime.utcfromtimestamp

INFINITY_MICROSECONDS = 2 ** 63 - 1
MINUS_INFINITY_MICROSECONDS = -1 * INFINITY_MICROSECONDS - 1


# data is 64-bit integer representing microseconds since 2000-01-01
def timestamp_recv_integer(data, offset, length):
    micros = q_unpack(data, offset)[0]
    try:
        return EPOCH + timedelta(microseconds=micros)
    except OverflowError:
        if micros == INFINITY_MICROSECONDS:
            return datetime.datetime.max
        elif micros == MINUS_INFINITY_MICROSECONDS:
            return datetime.datetime.min
        else:
            raise exc_info()[1]


# data is double-precision float representing seconds since 2000-01-01
def timestamp_recv_float(data, offset, length):
    return utcfromtimestamp(EPOCH_SECONDS + d_unpack(data, offset)[0])


# data is 64-bit integer representing microseconds since 2000-01-01
def timestamp_send_integer(v):
    if v == datetime.datetime.max:
        micros = INFINITY_MICROSECONDS
    elif v == datetime.datetime.min:
        micros = MINUS_INFINITY_MICROSECONDS
    else:
        micros = int(
            (timegm(v.timetuple()) - EPOCH_SECONDS) * 1e6) + v.microsecond
    return q_pack(micros)


# data is double-precision float representing seconds since 2000-01-01
def timestamp_send_float(v):
    return d_pack(timegm(v.timetuple) + v.microsecond / 1e6 - EPOCH_SECONDS)


def timestamptz_send_integer(v):
    # timestamps should be sent as UTC.  If they have zone info,
    # convert them.
    return timestamp_send_integer(v.astimezone(utc).replace(tzinfo=None))


def timestamptz_send_float(v):
    # timestamps should be sent as UTC.  If they have zone info,
    # convert them.
    return timestamp_send_float(v.astimezone(utc).replace(tzinfo=None))

DATETIME_MAX_TZ = datetime.datetime.max.replace(tzinfo=utc)
DATETIME_MIN_TZ = datetime.datetime.min.replace(tzinfo=utc)


# return a timezone-aware datetime instance if we're reading from a
# "timestamp with timezone" type.  The timezone returned will always be
# UTC, but providing that additional information can permit conversion
# to local.
def timestamptz_recv_integer(data, offset, length):
    micros = q_unpack(data, offset)[0]
    try:
        return EPOCH_TZ + timedelta(microseconds=micros)
    except OverflowError:
        if micros == INFINITY_MICROSECONDS:
            return DATETIME_MAX_TZ
        elif micros == MINUS_INFINITY_MICROSECONDS:
            return DATETIME_MIN_TZ
        else:
            raise exc_info()[1]


def timestamptz_recv_float(data, offset, length):
    return timestamp_recv_float(data, offset, length).replace(tzinfo=utc)


def interval_send_integer(v):
    microseconds = v.microseconds
    try:
        microseconds += int(v.seconds * 1e6)
    except AttributeError:
        pass

    try:
        months = v.months
    except AttributeError:
        months = 0

    return qii_pack(microseconds, v.days, months)


def interval_send_float(v):
    seconds = v.microseconds / 1000.0 / 1000.0
    try:
        seconds += v.seconds
    except AttributeError:
        pass

    try:
        months = v.months
    except AttributeError:
        months = 0

    return dii_pack(seconds, v.days, months)


def interval_recv_integer(data, offset, length):
    microseconds, days, months = qii_unpack(data, offset)
    if months == 0:
        seconds, micros = divmod(microseconds, 1e6)
        return datetime.timedelta(days, seconds, micros)
    else:
        return Interval(microseconds, days, months)


def interval_recv_float(data, offset, length):
    seconds, days, months = dii_unpack(data, offset)
    if months == 0:
        secs, microseconds = divmod(seconds, 1e6)
        return datetime.timedelta(days, secs, microseconds)
    else:
        return Interval(int(seconds * 1000 * 1000), days, months)


def int8_recv(data, offset, length):
    return q_unpack(data, offset)[0]


def int2_recv(data, offset, length):
    return h_unpack(data, offset)[0]


def int4_recv(data, offset, length):
    return i_unpack(data, offset)[0]


def float4_recv(data, offset, length):
    return f_unpack(data, offset)[0]


def float8_recv(data, offset, length):
    return d_unpack(data, offset)[0]


def bytea_send(v):
    return v

# bytea
if PY2:
    def bytea_recv(data, offset, length):
        return Bytea(data[offset:offset + length])
else:
    def bytea_recv(data, offset, length):
        return data[offset:offset + length]


def uuid_send(v):
    return v.bytes


def uuid_recv(data, offset, length):
    return UUID(bytes=data[offset:offset+length])


TRUE = b("\x01")
FALSE = b("\x00")


def bool_send(v):
    return TRUE if v else FALSE


NULL = i_pack(-1)

NULL_BYTE = b('\x00')


def null_send(v):
    return NULL


def int_in(data, offset, length):
    return int(data[offset: offset + length])


class Cursor():
    """A cursor object is returned by the :meth:`~Connection.cursor` method of
    a connection. It has the following attributes and methods:

    .. attribute:: arraysize

        This read/write attribute specifies the number of rows to fetch at a
        time with :meth:`fetchmany`.  It defaults to 1.

    .. attribute:: connection

        This read-only attribute contains a reference to the connection object
        (an instance of :class:`Connection`) on which the cursor was
        created.

        This attribute is part of a DBAPI 2.0 extension.  Accessing this
        attribute will generate the following warning: ``DB-API extension
        cursor.connection used``.

    .. attribute:: rowcount

        This read-only attribute contains the number of rows that the last
        ``execute()`` or ``executemany()`` method produced (for query
        statements like ``SELECT``) or affected (for modification statements
        like ``UPDATE``).

        The value is -1 if:

        - No ``execute()`` or ``executemany()`` method has been performed yet
          on the cursor.
        - There was no rowcount associated with the last ``execute()``.
        - At least one of the statements executed as part of an
          ``executemany()`` had no row count associated with it.
        - Using a ``SELECT`` query statement on PostgreSQL server older than
          version 9.
        - Using a ``COPY`` query statement on PostgreSQL server version 8.1 or
          older.

        This attribute is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.

    .. attribute:: description

        This read-only attribute is a sequence of 7-item sequences.  Each value
        contains information describing one result column.  The 7 items
        returned for each column are (name, type_code, display_size,
        internal_size, precision, scale, null_ok).  Only the first two values
        are provided by the current implementation.

        This attribute is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
    """

    def __init__(self, connection):
        self._c = connection
        self.arraysize = 1
        self.ps = None
        self._row_count = -1
        self._cached_rows = deque()
        self.portal_name = None
        self.portal_suspended = False

    @property
    def connection(self):
        warn("DB-API extension cursor.connection used", stacklevel=3)
        return self._c

    @property
    def rowcount(self):
        return self._row_count

    description = property(lambda self: self._getDescription())

    def _getDescription(self):
        if self.ps is None:
            return None
        row_desc = self.ps['row_desc']
        if len(row_desc) == 0:
            return None
        columns = []
        for col in row_desc:
            columns.append(
                (col["name"], col["type_oid"], None, None, None, None, None))
        return columns

    ##
    # Executes a database operation.  Parameters may be provided as a sequence
    # or mapping and will be bound to variables in the operation.
    # <p>
    # Stability: Part of the DBAPI 2.0 specification.
    def execute(self, operation, args=None, stream=None):
        """Executes a database operation.  Parameters may be provided as a
        sequence, or as a mapping, depending upon the value of
        :data:`pg8000.paramstyle`.

        This method is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.

        :param operation:
            The SQL statement to execute.

        :param args:
            If :data:`paramstyle` is ``qmark``, ``numeric``, or ``format``,
            this argument should be an array of parameters to bind into the
            statement.  If :data:`paramstyle` is ``named``, the argument should
            be a dict mapping of parameters.  If the :data:`paramstyle` is
            ``pyformat``, the argument value may be either an array or a
            mapping.

        :param stream: This is a pg8000 extension for use with the PostgreSQL
            `COPY
            <http://www.postgresql.org/docs/current/static/sql-copy.html>`_
            command. For a COPY FROM the parameter must be a readable file-like
            object, and for COPY TO it must be writable.

            .. versionadded:: 1.9.11
        """
        try:
            self._c._lock.acquire()
            self.stream = stream

            if not self._c.in_transaction and not self._c.autocommit:
                self._c.execute(self, "begin transaction", None)
            self._c.execute(self, operation, args)
        except AttributeError:
            if self._c is None:
                raise InterfaceError("Cursor closed")
            elif self._c._sock is None:
                raise InterfaceError("connection is closed")
            else:
                raise exc_info()[1]
        finally:
            self._c._lock.release()

    def executemany(self, operation, param_sets):
        """Prepare a database operation, and then execute it against all
        parameter sequences or mappings provided.

        This method is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.

        :param operation:
            The SQL statement to execute
        :param parameter_sets:
            A sequence of parameters to execute the statement with. The values
            in the sequence should be sequences or mappings of parameters, the
            same as the args argument of the :meth:`execute` method.
        """
        rowcounts = []
        for parameters in param_sets:
            self.execute(operation, parameters)
            rowcounts.append(self._row_count)

        self._row_count = -1 if -1 in rowcounts else sum(rowcounts)

    def fetchone(self):
        """Fetch the next row of a query result set.

        This method is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.

        :returns:
            A row as a sequence of field values, or ``None`` if no more rows
            are available.
        """
        try:
            return next(self)
        except StopIteration:
            return None
        except TypeError:
            raise ProgrammingError("attempting to use unexecuted cursor")
        except AttributeError:
            raise ProgrammingError("attempting to use unexecuted cursor")

    def fetchmany(self, num=None):
        """Fetches the next set of rows of a query result.

        This method is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.

        :param size:

            The number of rows to fetch when called.  If not provided, the
            :attr:`arraysize` attribute value is used instead.

        :returns:

            A sequence, each entry of which is a sequence of field values
            making up a row.  If no more rows are available, an empty sequence
            will be returned.
        """
        try:
            return tuple(
                islice(self, self.arraysize if num is None else num))
        except TypeError:
            raise ProgrammingError("attempting to use unexecuted cursor")

    def fetchall(self):
        """Fetches all remaining rows of a query result.

        This method is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.

        :returns:

            A sequence, each entry of which is a sequence of field values
            making up a row.
        """
        try:
            return tuple(self)
        except TypeError:
            raise ProgrammingError("attempting to use unexecuted cursor")

    def close(self):
        """Closes the cursor.

        This method is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        self._c = None

    def __iter__(self):
        """A cursor object is iterable to retrieve the rows from a query.

        This is a DBAPI 2.0 extension.
        """
        return self

    def setinputsizes(self, sizes):
        """This method is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_, however, it is not
        implemented by pg8000.
        """
        pass

    def setoutputsize(self, size, column=None):
        """This method is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_, however, it is not
        implemented by pg8000.
        """
        pass

    def __next__(self):
        try:
            self._c._lock.acquire()
            return self._cached_rows.popleft()
        except IndexError:
            if self.portal_suspended:
                self._c.send_EXECUTE(self)
                self._c._write(SYNC_MSG)
                self._c._flush()
                self._c.handle_messages(self)
                if not self.portal_suspended:
                    self._c.close_portal(self)
            try:
                return self._cached_rows.popleft()
            except IndexError:
                if self.ps is None:
                    raise ProgrammingError("A query hasn't been issued.")
                elif len(self.ps['row_desc']) == 0:
                    raise ProgrammingError("no result set")
                else:
                    raise StopIteration()
        finally:
            self._c._lock.release()

if PY2:
    Cursor.next = Cursor.__next__

# Message codes
NOTICE_RESPONSE = b("N")
AUTHENTICATION_REQUEST = b("R")
PARAMETER_STATUS = b("S")
BACKEND_KEY_DATA = b("K")
READY_FOR_QUERY = b("Z")
ROW_DESCRIPTION = b("T")
ERROR_RESPONSE = b("E")
DATA_ROW = b("D")
COMMAND_COMPLETE = b("C")
PARSE_COMPLETE = b("1")
BIND_COMPLETE = b("2")
CLOSE_COMPLETE = b("3")
PORTAL_SUSPENDED = b("s")
NO_DATA = b("n")
PARAMETER_DESCRIPTION = b("t")
NOTIFICATION_RESPONSE = b("A")
COPY_DONE = b("c")
COPY_DATA = b("d")
COPY_IN_RESPONSE = b("G")
COPY_OUT_RESPONSE = b("H")

BIND = b("B")
PARSE = b("P")
EXECUTE = b("E")
FLUSH = b('H')
SYNC = b('S')
PASSWORD = b('p')
DESCRIBE = b('D')
TERMINATE = b('X')
CLOSE = b('C')

FLUSH_MSG = FLUSH + i_pack(4)
SYNC_MSG = SYNC + i_pack(4)
TERMINATE_MSG = TERMINATE + i_pack(4)
COPY_DONE_MSG = COPY_DONE + i_pack(4)

# DESCRIBE constants
STATEMENT = b('S')
PORTAL = b('P')

# ErrorResponse codes
RESPONSE_SEVERITY = b("S")  # always present
RESPONSE_CODE = b("C")  # always present
RESPONSE_MSG = b("M")  # always present
RESPONSE_DETAIL = b("D")
RESPONSE_HINT = b("H")
RESPONSE_POSITION = b("P")
RESPONSE__POSITION = b("p")
RESPONSE__QUERY = b("q")
RESPONSE_WHERE = b("W")
RESPONSE_FILE = b("F")
RESPONSE_LINE = b("L")
RESPONSE_ROUTINE = b("R")

IDLE = b("I")
IDLE_IN_TRANSACTION = b("T")
IDLE_IN_FAILED_TRANSACTION = b("E")


# Byte1('N') - Identifier
# Int32 - Message length
# Any number of these, followed by a zero byte:
#   Byte1 - code identifying the field type (see responseKeys)
#   String - field value
def data_into_dict(data):
    return dict((s[0:1], s[1:]) for s in data.split(NULL_BYTE))

arr_trans = dict(zip(map(ord, u("[] 'u")), list(u('{}')) + [None] * 3))


class MulticastDelegate(object):
    def __init__(self):
        self.delegates = []

    def __iadd__(self, delegate):
        self.add(delegate)
        return self

    def add(self, delegate):
        self.delegates.append(delegate)

    def __isub__(self, delegate):
        self.delegates.remove(delegate)
        return self

    def __call__(self, *args, **kwargs):
        for d in self.delegates:
            d(*args, **kwargs)


class Connection(object):
    """A connection object is returned by the :func:`pg8000.connect` function.
    It represents a single physical connection to a PostgreSQL database.

    .. attribute:: Connection.notifies

        A list of server-side notifications received by this database
        connection (via the LISTEN/NOTIFY PostgreSQL commands).  Each list
        element is a two-element tuple containing the PostgreSQL backend PID
        that issued the notify, and the notification name.

        PostgreSQL will only send notifications to a client between
        transactions.  The contents of this property are generally only
        populated after a commit or rollback of the current transaction.

        This list can be modified by a client application to clean out
        notifications as they are handled.  However, inspecting or modifying
        this collection should only be done while holding the
        :attr:`notifies_lock` lock in order to guarantee thread-safety.

        This attribute is not part of the DBAPI standard; it is a pg8000
        extension.

        .. versionadded:: 1.07

    .. attribute:: Connection.notifies_lock

        A :class:`threading.Lock` object that should be held to read or
        modify the contents of the :attr:`notifies` list.

        This attribute is not part of the DBAPI standard; it is a pg8000
        extension.

        .. versionadded:: 1.07

    .. attribute:: Connection.autocommit

        Following the DB-API specification, autocommit is off by default.
        It can be turned on by setting this boolean pg8000-specific autocommit
        property to True.

        .. versionadded:: 1.9

    .. exception:: Connection.Error
                   Connection.Warning
                   Connection.InterfaceError
                   Connection.DatabaseError
                   Connection.InternalError
                   Connection.OperationalError
                   Connection.ProgrammingError
                   Connection.IntegrityError
                   Connection.DataError
                   Connection.NotSupportedError

        All of the standard database exception types are accessible via
        connection instances.

        This is a DBAPI 2.0 extension.  Accessing any of these attributes will
        generate the warning ``DB-API extension connection.DatabaseError
        used``.
    """

    # DBAPI Extension: supply exceptions as attributes on the connection
    Warning = property(lambda self: self._getError(Warning))
    Error = property(lambda self: self._getError(Error))
    InterfaceError = property(lambda self: self._getError(InterfaceError))
    DatabaseError = property(lambda self: self._getError(DatabaseError))
    OperationalError = property(lambda self: self._getError(OperationalError))
    IntegrityError = property(lambda self: self._getError(IntegrityError))
    InternalError = property(lambda self: self._getError(InternalError))
    ProgrammingError = property(lambda self: self._getError(ProgrammingError))
    NotSupportedError = property(
        lambda self: self._getError(NotSupportedError))

    # Determines the number of rows to read from the database server at once.
    # Reading more rows increases performance at the cost of memory.  The
    # default value is 100 rows.  The effect of this parameter is transparent.
    # That is, the library reads more rows when the cache is empty
    # automatically.
    _row_cache_size = 100
    _row_cache_size_bin = i_pack(_row_cache_size)

    def _getError(self, error):
        warn(
            "DB-API extension connection.%s used" %
            error.__name__, stacklevel=3)
        return error

    def __init__(self, user, host, unix_sock, port, database, password, ssl):
        self._client_encoding = "utf8"
        self._commands_with_count = (
            b("INSERT"), b("DELETE"), b("UPDATE"), b("MOVE"),
            b("FETCH"), b("COPY"), b("SELECT"))
        self._lock = threading.Lock()

        if user is None:
            try:
                self.user = os.environ['PGUSER']
            except KeyError:
                try:
                    self.user = os.environ['USER']
                except KeyError:
                    raise InterfaceError(
                        "The 'user' connection parameter was omitted, and "
                        "neither the PGUSER or USER environment variables "
                        "were set.")
        else:
            self.user = user

        if isinstance(self.user, text_type):
            self.user = self.user.encode('utf8')

        self.password = password
        self.autocommit = False
        self._xid = None

        self._caches = defaultdict(lambda: defaultdict(dict))
        self.statement_number = 0
        self.portal_number = 0

        try:
            if unix_sock is None and host is not None:
                self._usock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            elif unix_sock is not None:
                if not hasattr(socket, "AF_UNIX"):
                    raise InterfaceError(
                        "attempt to connect to unix socket on unsupported "
                        "platform")
                self._usock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            else:
                raise ProgrammingError(
                    "one of host or unix_sock must be provided")
            if unix_sock is None and host is not None:
                self._usock.connect((host, port))
            elif unix_sock is not None:
                self._usock.connect(unix_sock)

            if ssl:
                try:
                    self._lock.acquire()
                    import ssl as sslmodule
                    # Int32(8) - Message length, including self.
                    # Int32(80877103) - The SSL request code.
                    self._usock.sendall(ii_pack(8, 80877103))
                    resp = self._usock.recv(1)
                    if resp == b('S'):
                        self._usock = sslmodule.wrap_socket(self._usock)
                    else:
                        raise InterfaceError("Server refuses SSL")
                except ImportError:
                    raise InterfaceError(
                        "SSL required but ssl module not available in "
                        "this python installation")
                finally:
                    self._lock.release()

            self._sock = self._usock.makefile(mode="rwb")
        except socket.error:
            self._usock.close()
            raise InterfaceError("communication error", exc_info()[1])
        self._flush = self._sock.flush
        self._read = self._sock.read

        if PRE_26:
            self._write = self._sock.writelines
        else:
            self._write = self._sock.write
        self._backend_key_data = None

        ##
        # An event handler that is fired when the database server issues a
        # notice.
        # The value of this property is a MulticastDelegate. A callback
        # can be added by using connection.NotificationReceived += SomeMethod.
        # The method will be called with a single argument, an object that has
        # properties: severity, code, msg, and possibly others (detail, hint,
        # position, where, file, line, and routine). Callbacks can be removed
        # with the -= operator.
        # <p>
        # Stability: Added in v1.03, stability guaranteed for v1.xx.
        self.NoticeReceived = MulticastDelegate()

        ##
        # An event handler that is fired when a runtime configuration option is
        # changed on the server.  The value of this property is a
        # MulticastDelegate.  A callback can be added by using
        # connection.NotificationReceived += SomeMethod. Callbacks can be
        # removed with the -= operator. The method will be called with a single
        # argument, an object that has properties "key" and "value".
        # <p>
        # Stability: Added in v1.03, stability guaranteed for v1.xx.
        self.ParameterStatusReceived = MulticastDelegate()

        ##
        # An event handler that is fired when NOTIFY occurs for a notification
        # that has been LISTEN'd for.  The value of this property is a
        # MulticastDelegate.  A callback can be added by using
        # connection.NotificationReceived += SomeMethod. The method will be
        # called with a single argument, an object that has properties:
        # backend_pid, condition, and additional_info. Callbacks can be
        # removed with the -= operator.
        # <p>
        # Stability: Added in v1.03, stability guaranteed for v1.xx.
        self.NotificationReceived = MulticastDelegate()

        self.ParameterStatusReceived += self.handle_PARAMETER_STATUS

        def text_out(v):
            return v.encode(self._client_encoding)

        def time_out(v):
            return v.isoformat().encode(self._client_encoding)

        def date_out(v):
            if v == datetime.date.max:
                return 'infinity'.encode(self._client_encoding)
            elif v == datetime.date.min:
                return '-infinity'.encode(self._client_encoding)
            else:
                return v.isoformat().encode(self._client_encoding)

        def unknown_out(v):
            return str(v).encode(self._client_encoding)

        trans_tab = dict(zip(map(ord, u('{}')), u('[]')))
        glbls = {'Decimal': Decimal}

        def array_in(data, idx, length):
            arr = []
            prev_c = None
            for c in data[idx:idx+length].decode(
                    self._client_encoding).translate(
                    trans_tab).replace(u('NULL'), u('None')):
                if c not in ('[', ']', ',', 'N') and prev_c in ('[', ','):
                    arr.extend("Decimal('")
                elif c in (']', ',') and prev_c not in ('[', ']', ',', 'e'):
                    arr.extend("')")

                arr.append(c)
                prev_c = c
            return eval(''.join(arr), glbls)

        def array_recv(data, idx, length):
            final_idx = idx + length
            dim, hasnull, typeoid = iii_unpack(data, idx)
            idx += 12

            # get type conversion method for typeoid
            conversion = self.pg_types[typeoid][1]

            # Read dimension info
            dim_lengths = []
            for i in range(dim):
                dim_lengths.append(ii_unpack(data, idx)[0])
                idx += 8

            # Read all array values
            values = []
            while idx < final_idx:
                element_len, = i_unpack(data, idx)
                idx += 4
                if element_len == -1:
                    values.append(None)
                else:
                    values.append(conversion(data, idx, element_len))
                    idx += element_len

            # at this point, {{1,2,3},{4,5,6}}::int[][] looks like
            # [1,2,3,4,5,6]. go through the dimensions and fix up the array
            # contents to match expected dimensions
            for length in reversed(dim_lengths[1:]):
                values = list(map(list, zip(*[iter(values)] * length)))
            return values

        def vector_in(data, idx, length):
            return eval('[' + data[idx:idx+length].decode(
                self._client_encoding).replace(' ', ',') + ']')

        if PY2:
            def text_recv(data, offset, length):
                return unicode(  # noqa
                    data[offset: offset + length], self._client_encoding)

            def bool_recv(d, o, l):
                return d[o] == "\x01"

            def json_in(data, offset, length):
                return loads(unicode(  # noqa
                    data[offset: offset + length], self._client_encoding))

        else:
            def text_recv(data, offset, length):
                return str(
                    data[offset: offset + length], self._client_encoding)

            def bool_recv(data, offset, length):
                return data[offset] == 1

            def json_in(data, offset, length):
                return loads(
                    str(data[offset: offset + length], self._client_encoding))

        def time_in(data, offset, length):
            hour = int(data[offset:offset + 2])
            minute = int(data[offset + 3:offset + 5])
            sec = Decimal(
                data[offset + 6:offset + length].decode(self._client_encoding))
            return datetime.time(
                hour, minute, int(sec), int((sec - int(sec)) * 1000000))

        def date_in(data, offset, length):
            year_str = data[offset:offset + 4].decode(self._client_encoding)
            if year_str == 'infi':
                return datetime.date.max
            elif year_str == '-inf':
                return datetime.date.min
            else:
                return datetime.date(
                    int(year_str), int(data[offset + 5:offset + 7]),
                    int(data[offset + 8:offset + 10]))

        def numeric_in(data, offset, length):
            return Decimal(
                data[offset: offset + length].decode(self._client_encoding))

        def numeric_out(d):
            return str(d).encode(self._client_encoding)

        self.pg_types = defaultdict(
            lambda: (FC_TEXT, text_recv), {
                16: (FC_BINARY, bool_recv),  # boolean
                17: (FC_BINARY, bytea_recv),  # bytea
                19: (FC_BINARY, text_recv),  # name type
                20: (FC_BINARY, int8_recv),  # int8
                21: (FC_BINARY, int2_recv),  # int2
                22: (FC_TEXT, vector_in),  # int2vector
                23: (FC_BINARY, int4_recv),  # int4
                25: (FC_BINARY, text_recv),  # TEXT type
                26: (FC_TEXT, int_in),  # oid
                28: (FC_TEXT, int_in),  # xid
                114: (FC_TEXT, json_in),  # json
                700: (FC_BINARY, float4_recv),  # float4
                701: (FC_BINARY, float8_recv),  # float8
                705: (FC_BINARY, text_recv),  # unknown
                829: (FC_TEXT, text_recv),  # MACADDR type
                1000: (FC_BINARY, array_recv),  # BOOL[]
                1003: (FC_BINARY, array_recv),  # NAME[]
                1005: (FC_BINARY, array_recv),  # INT2[]
                1007: (FC_BINARY, array_recv),  # INT4[]
                1009: (FC_BINARY, array_recv),  # TEXT[]
                1014: (FC_BINARY, array_recv),  # CHAR[]
                1015: (FC_BINARY, array_recv),  # VARCHAR[]
                1016: (FC_BINARY, array_recv),  # INT8[]
                1021: (FC_BINARY, array_recv),  # FLOAT4[]
                1022: (FC_BINARY, array_recv),  # FLOAT8[]
                1042: (FC_BINARY, text_recv),  # CHAR type
                1043: (FC_BINARY, text_recv),  # VARCHAR type
                1082: (FC_TEXT, date_in),  # date
                1083: (FC_TEXT, time_in),
                1114: (FC_BINARY, timestamp_recv_float),  # timestamp w/ tz
                1184: (FC_BINARY, timestamptz_recv_float),
                1186: (FC_BINARY, interval_recv_integer),
                1231: (FC_TEXT, array_in),  # NUMERIC[]
                1263: (FC_BINARY, array_recv),  # cstring[]
                1700: (FC_TEXT, numeric_in),  # NUMERIC
                2275: (FC_BINARY, text_recv),  # cstring
                2950: (FC_BINARY, uuid_recv),  # uuid
                3802: (FC_TEXT, json_in),  # jsonb
            })

        self.py_types = {
            type(None): (-1, FC_BINARY, null_send),  # null
            bool: (16, FC_BINARY, bool_send),
            int: (705, FC_TEXT, unknown_out),
            float: (701, FC_BINARY, d_pack),  # float8
            str: (705, FC_TEXT, text_out),  # unknown
            datetime.date: (1082, FC_TEXT, date_out),  # date
            datetime.time: (1083, FC_TEXT, time_out),  # time
            1114: (1114, FC_BINARY, timestamp_send_integer),  # timestamp
            # timestamp w/ tz
            1184: (1184, FC_BINARY, timestamptz_send_integer),
            datetime.timedelta: (1186, FC_BINARY, interval_send_integer),
            Interval: (1186, FC_BINARY, interval_send_integer),
            Decimal: (1700, FC_TEXT, numeric_out),  # Decimal
            UUID: (2950, FC_BINARY, uuid_send),  # uuid
        }

        self.inspect_funcs = {
            datetime.datetime: self.inspect_datetime,
            list: self.array_inspect,
            tuple: self.array_inspect,
        }

        if PY2:
            self.py_types[Bytea] = (17, FC_BINARY, bytea_send)  # bytea
            self.py_types[text_type] = (705, FC_TEXT, text_out)  # unknown

            self.py_types[long] = (705, FC_TEXT, unknown_out)  # noqa
        else:
            self.py_types[bytes] = (17, FC_BINARY, bytea_send)  # bytea

        try:
            from ipaddress import (
                ip_address, IPv4Address, IPv6Address, ip_network, IPv4Network,
                IPv6Network)

            def inet_out(v):
                return str(v).encode(self._client_encoding)

            def inet_in(data, offset, length):
                inet_str = data[offset: offset + length].decode(
                    self._client_encoding)
                if '/' in inet_str:
                    return ip_network(inet_str, False)
                else:
                    return ip_address(inet_str)

            self.py_types[IPv4Address] = (869, FC_TEXT, inet_out)  # inet
            self.py_types[IPv6Address] = (869, FC_TEXT, inet_out)  # inet
            self.py_types[IPv4Network] = (869, FC_TEXT, inet_out)  # inet
            self.py_types[IPv6Network] = (869, FC_TEXT, inet_out)  # inet
            self.pg_types[869] = (FC_TEXT, inet_in)  # inet
        except ImportError:
            pass

        self.message_types = {
            NOTICE_RESPONSE: self.handle_NOTICE_RESPONSE,
            AUTHENTICATION_REQUEST: self.handle_AUTHENTICATION_REQUEST,
            PARAMETER_STATUS: self.handle_PARAMETER_STATUS,
            BACKEND_KEY_DATA: self.handle_BACKEND_KEY_DATA,
            READY_FOR_QUERY: self.handle_READY_FOR_QUERY,
            ROW_DESCRIPTION: self.handle_ROW_DESCRIPTION,
            ERROR_RESPONSE: self.handle_ERROR_RESPONSE,
            DATA_ROW: self.handle_DATA_ROW,
            COMMAND_COMPLETE: self.handle_COMMAND_COMPLETE,
            PARSE_COMPLETE: self.handle_PARSE_COMPLETE,
            BIND_COMPLETE: self.handle_BIND_COMPLETE,
            CLOSE_COMPLETE: self.handle_CLOSE_COMPLETE,
            PORTAL_SUSPENDED: self.handle_PORTAL_SUSPENDED,
            NO_DATA: self.handle_NO_DATA,
            PARAMETER_DESCRIPTION: self.handle_PARAMETER_DESCRIPTION,
            NOTIFICATION_RESPONSE: self.handle_NOTIFICATION_RESPONSE,
            COPY_DONE: self.handle_COPY_DONE,
            COPY_DATA: self.handle_COPY_DATA,
            COPY_IN_RESPONSE: self.handle_COPY_IN_RESPONSE,
            COPY_OUT_RESPONSE: self.handle_COPY_OUT_RESPONSE}

        # Int32 - Message length, including self.
        # Int32(196608) - Protocol version number.  Version 3.0.
        # Any number of key/value pairs, terminated by a zero byte:
        #   String - A parameter name (user, database, or options)
        #   String - Parameter value
        protocol = 196608
        val = bytearray(
            i_pack(protocol) + b("user\x00") + self.user + NULL_BYTE)
        if database is not None:
            if isinstance(database, text_type):
                database = database.encode('utf8')
            val.extend(b("database\x00") + database + NULL_BYTE)
        val.append(0)
        self._write(i_pack(len(val) + 4))
        self._write(val)
        self._flush()

        self._cursor = self.cursor()
        try:
            self._lock.acquire()
            code = self.error = None
            while code not in (READY_FOR_QUERY, ERROR_RESPONSE):
                code, data_len = ci_unpack(self._read(5))
                self.message_types[code](self._read(data_len - 4), None)
            if self.error is not None:
                raise self.error
        except:
            self._close()
            raise
        finally:
            self._lock.release()

        self.in_transaction = False
        self.notifies = []
        self.notifies_lock = threading.Lock()

    def handle_ERROR_RESPONSE(self, data, ps):
        msg_dict = data_into_dict(data)
        if msg_dict[RESPONSE_CODE] == "28000":
            self.error = InterfaceError("md5 password authentication failed")
        else:
            self.error = ProgrammingError(
                msg_dict[RESPONSE_SEVERITY], msg_dict[RESPONSE_CODE],
                msg_dict[RESPONSE_MSG])

    def handle_CLOSE_COMPLETE(self, data, ps):
        pass

    def handle_PARSE_COMPLETE(self, data, ps):
        # Byte1('1') - Identifier.
        # Int32(4) - Message length, including self.
        pass

    def handle_BIND_COMPLETE(self, data, ps):
        pass

    def handle_PORTAL_SUSPENDED(self, data, cursor):
        cursor.portal_suspended = True

    def handle_PARAMETER_DESCRIPTION(self, data, ps):
        # Well, we don't really care -- we're going to send whatever we
        # want and let the database deal with it.  But thanks anyways!

        # count = h_unpack(data)[0]
        # type_oids = unpack_from("!" + "i" * count, data, 2)
        pass

    def handle_COPY_DONE(self, data, ps):
        self._copy_done = True

    def handle_COPY_OUT_RESPONSE(self, data, ps):
        # Int8(1) - 0 textual, 1 binary
        # Int16(2) - Number of columns
        # Int16(N) - Format codes for each column (0 text, 1 binary)

        is_binary, num_cols = bh_unpack(data)
        # column_formats = unpack_from('!' + 'h' * num_cols, data, 3)
        if ps.stream is None:
            raise InterfaceError(
                "An output stream is required for the COPY OUT response.")

    def handle_COPY_DATA(self, data, ps):
        ps.stream.write(data)

    def handle_COPY_IN_RESPONSE(self, data, ps):
        # Int16(2) - Number of columns
        # Int16(N) - Format codes for each column (0 text, 1 binary)
        is_binary, num_cols = bh_unpack(data)
        # column_formats = unpack_from('!' + 'h' * num_cols, data, 3)
        assert self._lock.locked()
        if ps.stream is None:
            raise InterfaceError(
                "An input stream is required for the COPY IN response.")

        if PY2:
            while True:
                data = ps.stream.read(8192)
                if not data:
                    break
                self._write(COPY_DATA + i_pack(len(data) + 4))
                self._write(data)
                self._flush()
        else:
            bffr = bytearray(8192)
            while True:
                bytes_read = ps.stream.readinto(bffr)
                if bytes_read == 0:
                    break
                self._write(COPY_DATA + i_pack(bytes_read + 4))
                self._write(bffr[:bytes_read])
                self._flush()

        # Send CopyDone
        # Byte1('c') - Identifier.
        # Int32(4) - Message length, including self.
        self._write(COPY_DONE_MSG)
        self._write(SYNC_MSG)
        self._flush()

    def handle_NOTIFICATION_RESPONSE(self, data, ps):
        self.NotificationReceived(data)
        ##
        # A message sent if this connection receives a NOTIFY that it was
        # LISTENing for.
        # <p>
        # Stability: Added in pg8000 v1.03.  When limited to accessing
        # properties from a notification event dispatch, stability is
        # guaranteed for v1.xx.
        backend_pid = i_unpack(data)[0]
        idx = 4
        null = data.find(NULL_BYTE, idx) - idx
        condition = data[idx:idx + null].decode("ascii")
        idx += null + 1
        null = data.find(NULL_BYTE, idx) - idx
        # additional_info = data[idx:idx + null]

        # psycopg2 compatible notification interface
        try:
            self.notifies_lock.acquire()
            self.notifies.append((backend_pid, condition))
        finally:
            self.notifies_lock.release()

    def cursor(self):
        """Creates a :class:`Cursor` object bound to this
        connection.

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        return Cursor(self)

    def commit(self):
        """Commits the current database transaction.

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        try:
            self._lock.acquire()
            self.execute(self._cursor, "commit", None)
        finally:
            self._lock.release()

    def rollback(self):
        """Rolls back the current database transaction.

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        try:
            self._lock.acquire()
            self.execute(self._cursor, "rollback", None)
        finally:
            self._lock.release()

    def _close(self):
        try:
            # Byte1('X') - Identifies the message as a terminate message.
            # Int32(4) - Message length, including self.
            self._write(TERMINATE_MSG)
            self._flush()
            self._sock.close()
            self._usock.close()
            self._sock = None
        except AttributeError:
            raise InterfaceError("connection is closed")
        except ValueError:
            raise InterfaceError("connection is closed")

    def close(self):
        """Closes the database connection.

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        try:
            self._lock.acquire()
            self._close()
        finally:
            self._lock.release()

    def handle_AUTHENTICATION_REQUEST(self, data, cursor):
        assert self._lock.locked()
        # Int32 -   An authentication code that represents different
        #           authentication messages:
        #               0 = AuthenticationOk
        #               5 = MD5 pwd
        #               2 = Kerberos v5 (not supported by pg8000)
        #               3 = Cleartext pwd (not supported by pg8000)
        #               4 = crypt() pwd (not supported by pg8000)
        #               6 = SCM credential (not supported by pg8000)
        #               7 = GSSAPI (not supported by pg8000)
        #               8 = GSSAPI data (not supported by pg8000)
        #               9 = SSPI (not supported by pg8000)
        # Some authentication messages have additional data following the
        # authentication code.  That data is documented in the appropriate
        # class.
        auth_code = i_unpack(data)[0]
        if auth_code == 0:
            pass
        elif auth_code == 3:
            if self.password is None:
                raise InterfaceError(
                    "server requesting password authentication, but no "
                    "password was provided")
            self._send_message(
                PASSWORD, self.password.encode("ascii") + NULL_BYTE)
            self._flush()
        elif auth_code == 5:
            ##
            # A message representing the backend requesting an MD5 hashed
            # password response.  The response will be sent as
            # md5(md5(pwd + login) + salt).

            # Additional message data:
            #  Byte4 - Hash salt.
            salt = b("").join(cccc_unpack(data, 4))
            if self.password is None:
                raise InterfaceError(
                    "server requesting MD5 password authentication, but no "
                    "password was provided")
            pwd = b("md5") + md5(
                md5(self.password.encode("ascii") + self.user).
                hexdigest().encode("ascii") + salt).hexdigest().encode("ascii")
            # Byte1('p') - Identifies the message as a password message.
            # Int32 - Message length including self.
            # String - The password.  Password may be encrypted.
            self._send_message(PASSWORD, pwd + NULL_BYTE)
            self._flush()

        elif auth_code in (2, 4, 6, 7, 8, 9):
            raise InterfaceError(
                "Authentication method " + str(auth_code) +
                " not supported by pg8000.")
        else:
            raise InterfaceError(
                "Authentication method " + str(auth_code) +
                " not recognized by pg8000.")

    def handle_READY_FOR_QUERY(self, data, ps):
        # Byte1 -   Status indicator.
        self.in_transaction = data != IDLE

    def handle_BACKEND_KEY_DATA(self, data, ps):
        self._backend_key_data = data

    def inspect_datetime(self, value):
        if value.tzinfo is None:
            return self.py_types[1114]  # timestamp
        else:
            return self.py_types[1184]  # send as timestamptz

    def make_params(self, values):
        params = []
        for value in values:
            typ = type(value)
            try:
                params.append(self.py_types[typ])
            except KeyError:
                try:
                    params.append(self.inspect_funcs[typ](value))
                except KeyError:
                    raise NotSupportedError(
                        "type " + str(exc_info()[1]) +
                        "not mapped to pg type")
        return params

    def handle_ROW_DESCRIPTION(self, data, cursor):
        count = h_unpack(data)[0]
        idx = 2
        for i in range(count):
            name = data[idx:data.find(NULL_BYTE, idx)]
            idx += len(name) + 1
            field = dict(
                zip((
                    "table_oid", "column_attrnum", "type_oid", "type_size",
                    "type_modifier", "format"), ihihih_unpack(data, idx)))
            field['name'] = name
            idx += 18
            cursor.ps['row_desc'].append(field)
            field['pg8000_fc'], field['func'] = \
                self.pg_types[field['type_oid']]

    def execute(self, cursor, operation, vals):
        if vals is None:
            vals = ()
        from . import paramstyle
        cache = self._caches[paramstyle]

        try:
            statement, make_args = cache['statement'][operation]
        except KeyError:
            statement, make_args = convert_paramstyle(paramstyle, operation)
            cache['statement'][operation] = statement, make_args

        args = make_args(vals)
        params = self.make_params(args)

        key = tuple(oid for oid, x, y in params), operation

        try:
            ps = cache['ps'][key]
            cursor.ps = ps
        except KeyError:
            statement_name = "pg8000_statement_" + str(self.statement_number)
            self.statement_number += 1
            statement_name_bin = statement_name.encode('ascii') + NULL_BYTE
            ps = {
                'row_desc': [],
                'param_funcs': tuple(x[2] for x in params),
            }
            cursor.ps = ps

            param_fcs = tuple(x[1] for x in params)

            # Byte1('P') - Identifies the message as a Parse command.
            # Int32 -   Message length, including self.
            # String -  Prepared statement name. An empty string selects the
            #           unnamed prepared statement.
            # String -  The query string.
            # Int16 -   Number of parameter data types specified (can be zero).
            # For each parameter:
            #   Int32 - The OID of the parameter data type.
            val = bytearray(statement_name_bin)
            val.extend(statement.encode(self._client_encoding) + NULL_BYTE)
            val.extend(h_pack(len(params)))
            for oid, fc, send_func in params:
                # Parse message doesn't seem to handle the -1 type_oid for NULL
                # values that other messages handle.  So we'll provide type_oid
                # 705, the PG "unknown" type.
                val.extend(i_pack(705 if oid == -1 else oid))

            # Byte1('D') - Identifies the message as a describe command.
            # Int32 - Message length, including self.
            # Byte1 - 'S' for prepared statement, 'P' for portal.
            # String - The name of the item to describe.
            self._send_message(PARSE, val)
            self._send_message(DESCRIBE, STATEMENT + statement_name_bin)
            self._write(SYNC_MSG)

            try:
                self._flush()
            except AttributeError:
                if self._sock is None:
                    raise InterfaceError("connection is closed")
                else:
                    raise exc_info()[1]

            self.handle_messages(cursor)

            # We've got row_desc that allows us to identify what we're
            # going to get back from this statement.
            output_fc = tuple(
                self.pg_types[f['type_oid']][0] for f in ps['row_desc'])

            ps['input_funcs'] = tuple(f['func'] for f in ps['row_desc'])
            # Byte1('B') - Identifies the Bind command.
            # Int32 - Message length, including self.
            # String - Name of the destination portal.
            # String - Name of the source prepared statement.
            # Int16 - Number of parameter format codes.
            # For each parameter format code:
            #   Int16 - The parameter format code.
            # Int16 - Number of parameter values.
            # For each parameter value:
            #   Int32 - The length of the parameter value, in bytes, not
            #           including this length.  -1 indicates a NULL parameter
            #           value, in which no value bytes follow.
            #   Byte[n] - Value of the parameter.
            # Int16 - The number of result-column format codes.
            # For each result-column format code:
            #   Int16 - The format code.
            ps['bind_1'] = statement_name_bin + h_pack(len(params)) + \
                pack("!" + "h" * len(param_fcs), *param_fcs) + \
                h_pack(len(params))

            ps['bind_2'] = h_pack(len(output_fc)) + \
                pack("!" + "h" * len(output_fc), *output_fc)

            cache['ps'][key] = ps

        cursor._cached_rows.clear()
        cursor._row_count = -1
        cursor.portal_name = "pg8000_portal_" + str(self.portal_number)
        self.portal_number += 1
        cursor.portal_name_bin = cursor.portal_name.encode('ascii') + NULL_BYTE
        cursor.execute_msg = cursor.portal_name_bin + \
            Connection._row_cache_size_bin

        # Byte1('B') - Identifies the Bind command.
        # Int32 - Message length, including self.
        # String - Name of the destination portal.
        # String - Name of the source prepared statement.
        # Int16 - Number of parameter format codes.
        # For each parameter format code:
        #   Int16 - The parameter format code.
        # Int16 - Number of parameter values.
        # For each parameter value:
        #   Int32 - The length of the parameter value, in bytes, not
        #           including this length.  -1 indicates a NULL parameter
        #           value, in which no value bytes follow.
        #   Byte[n] - Value of the parameter.
        # Int16 - The number of result-column format codes.
        # For each result-column format code:
        #   Int16 - The format code.
        retval = bytearray(cursor.portal_name_bin + ps['bind_1'])
        for value, send_func in zip(args, ps['param_funcs']):
            if value is None:
                val = NULL
            else:
                val = send_func(value)
                retval.extend(i_pack(len(val)))
            retval.extend(val)
        retval.extend(ps['bind_2'])

        self._send_message(BIND, retval)
        self.send_EXECUTE(cursor)
        self._write(SYNC_MSG)
        self._flush()
        self.handle_messages(cursor)
        if cursor.portal_suspended:
            if self.autocommit:
                raise InterfaceError(
                    "With autocommit on, it's not possible to retrieve more "
                    "rows than the pg8000 cache size, as the portal is closed "
                    "when the transaction is closed.")

        else:
            self.close_portal(cursor)

    def _send_message(self, code, data):
        try:
            self._write(code)
            self._write(i_pack(len(data) + 4))
            self._write(data)
            self._write(FLUSH_MSG)
        except ValueError:
            if str(exc_info()[1]) == "write to closed file":
                raise InterfaceError("connection is closed")
            else:
                raise exc_info()[1]
        except AttributeError:
            raise InterfaceError("connection is closed")

    def send_EXECUTE(self, cursor):
        # Byte1('E') - Identifies the message as an execute message.
        # Int32 -   Message length, including self.
        # String -  The name of the portal to execute.
        # Int32 -   Maximum number of rows to return, if portal
        #           contains a query # that returns rows.
        #           0 = no limit.
        cursor.portal_suspended = False
        self._send_message(EXECUTE, cursor.execute_msg)

    def handle_NO_DATA(self, msg, ps):
        pass

    def handle_COMMAND_COMPLETE(self, data, cursor):
        values = data[:-1].split(BINARY_SPACE)
        command = values[0]
        if command in self._commands_with_count:
            row_count = int(values[-1])
            if cursor._row_count == -1:
                cursor._row_count = row_count
            else:
                cursor._row_count += row_count

        if command in DDL_COMMANDS:
            for k in self._caches:
                self._caches[k]['ps'].clear()

    def handle_DATA_ROW(self, data, cursor):
        data_idx = 2
        row = []
        for func in cursor.ps['input_funcs']:
            vlen = i_unpack(data, data_idx)[0]
            data_idx += 4
            if vlen == -1:
                row.append(None)
            else:
                row.append(func(data, data_idx, vlen))
                data_idx += vlen
        cursor._cached_rows.append(row)

    def handle_messages(self, cursor):
        code = self.error = None

        try:
            while code != READY_FOR_QUERY:
                code, data_len = ci_unpack(self._read(5))
                self.message_types[code](self._read(data_len - 4), cursor)
        except:
            self._close()
            raise

        if self.error is not None:
            raise self.error

    # Byte1('C') - Identifies the message as a close command.
    # Int32 - Message length, including self.
    # Byte1 - 'S' for prepared statement, 'P' for portal.
    # String - The name of the item to close.
    def close_portal(self, cursor):
        self._send_message(CLOSE, PORTAL + cursor.portal_name_bin)
        self._write(SYNC_MSG)
        self._flush()
        self.handle_messages(cursor)

    def handle_NOTICE_RESPONSE(self, data, ps):
        resp = data_into_dict(data)
        self.NoticeReceived(resp)

    def handle_PARAMETER_STATUS(self, data, ps):
        pos = data.find(NULL_BYTE)
        key, value = data[:pos], data[pos + 1:-1]
        if key == b("client_encoding"):
            encoding = value.decode("ascii").lower()
            self._client_encoding = pg_to_py_encodings.get(encoding, encoding)

        elif key == b("integer_datetimes"):
            if value == b('on'):

                self.py_types[1114] = (1114, FC_BINARY, timestamp_send_integer)
                self.pg_types[1114] = (FC_BINARY, timestamp_recv_integer)

                self.py_types[1184] = (
                    1184, FC_BINARY, timestamptz_send_integer)
                self.pg_types[1184] = (FC_BINARY, timestamptz_recv_integer)

                self.py_types[Interval] = (
                    1186, FC_BINARY, interval_send_integer)
                self.py_types[datetime.timedelta] = (
                    1186, FC_BINARY, interval_send_integer)
                self.pg_types[1186] = (FC_BINARY, interval_recv_integer)
            else:
                self.py_types[1114] = (1114, FC_BINARY, timestamp_send_float)
                self.pg_types[1114] = (FC_BINARY, timestamp_recv_float)
                self.py_types[1184] = (1184, FC_BINARY, timestamptz_send_float)
                self.pg_types[1184] = (FC_BINARY, timestamptz_recv_float)

                self.py_types[Interval] = (
                    1186, FC_BINARY, interval_send_float)
                self.py_types[datetime.timedelta] = (
                    1186, FC_BINARY, interval_send_float)
                self.pg_types[1186] = (FC_BINARY, interval_recv_float)

        elif key == b("server_version"):
            self._server_version = LooseVersion(value.decode('ascii'))
            if self._server_version < LooseVersion('8.2.0'):
                self._commands_with_count = (
                    b("INSERT"), b("DELETE"), b("UPDATE"), b("MOVE"),
                    b("FETCH"))
            elif self._server_version < LooseVersion('9.0.0'):
                self._commands_with_count = (
                    b("INSERT"), b("DELETE"), b("UPDATE"), b("MOVE"),
                    b("FETCH"), b("COPY"))

    def array_inspect(self, value):
        # Check if array has any values.  If not, we can't determine the proper
        # array oid.
        first_element = array_find_first_element(value)
        if first_element is None:
            raise ArrayContentEmptyError("array has no values")

        # supported array output
        typ = type(first_element)

        if issubclass(typ, integer_types):
            # special int array support -- send as smallest possible array type
            typ = integer_types
            int2_ok, int4_ok, int8_ok = True, True, True
            for v in array_flatten(value):
                if v is None:
                    continue
                if min_int2 < v < max_int2:
                    continue
                int2_ok = False
                if min_int4 < v < max_int4:
                    continue
                int4_ok = False
                if min_int8 < v < max_int8:
                    continue
                int8_ok = False
            if int2_ok:
                array_oid = 1005  # INT2[]
                oid, fc, send_func = (21, FC_BINARY, h_pack)
            elif int4_ok:
                array_oid = 1007  # INT4[]
                oid, fc, send_func = (23, FC_BINARY, i_pack)
            elif int8_ok:
                array_oid = 1016  # INT8[]
                oid, fc, send_func = (20, FC_BINARY, q_pack)
            else:
                raise ArrayContentNotSupportedError(
                    "numeric not supported as array contents")
        else:
            try:
                oid, fc, send_func = self.make_params((first_element,))[0]

                # If unknown, assume it's a string array
                if oid == 705:
                    oid = 25
                    # Use binary ARRAY format to avoid having to properly
                    # escape text in the array literals
                    fc = FC_BINARY
                array_oid = pg_array_types[oid]
            except KeyError:
                raise ArrayContentNotSupportedError(
                    "oid " + str(oid) + " not supported as array contents")
            except NotSupportedError:
                raise ArrayContentNotSupportedError(
                    "type " + str(typ) + " not supported as array contents")

        if fc == FC_BINARY:
            def send_array(arr):
                # check for homogenous array
                for a, i, v in walk_array(arr):
                    if not isinstance(v, (typ, type(None))):
                        raise ArrayContentNotHomogenousError(
                            "not all array elements are of type " + str(typ))

                # check that all array dimensions are consistent
                array_check_dimensions(arr)

                has_null = array_has_null(arr)
                dim_lengths = array_dim_lengths(arr)
                data = bytearray(iii_pack(len(dim_lengths), has_null, oid))
                for i in dim_lengths:
                    data.extend(ii_pack(i, 1))
                for v in array_flatten(arr):
                    if v is None:
                        data += i_pack(-1)
                    else:
                        inner_data = send_func(v)
                        data += i_pack(len(inner_data))
                        data += inner_data
                return data
        else:
            def send_array(arr):
                for a, i, v in walk_array(arr):
                    if not isinstance(v, (typ, type(None))):
                        raise ArrayContentNotHomogenousError(
                            "not all array elements are of type " + str(typ))
                array_check_dimensions(arr)
                ar = deepcopy(arr)
                for a, i, v in walk_array(ar):
                    if v is None:
                        a[i] = 'NULL'
                    else:
                        a[i] = send_func(v).decode('ascii')

                return u(str(ar)).translate(arr_trans).encode('ascii')
        return (array_oid, fc, send_array)

    def xid(self, format_id, global_transaction_id, branch_qualifier):
        """Create a Transaction IDs (only global_transaction_id is used in pg)
        format_id and branch_qualifier are not used in postgres
        global_transaction_id may be any string identifier supported by
        postgres returns a tuple
        (format_id, global_transaction_id, branch_qualifier)"""
        return (format_id, global_transaction_id, branch_qualifier)

    def tpc_begin(self, xid):
        """Begins a TPC transaction with the given transaction ID xid.

        This method should be called outside of a transaction (i.e. nothing may
        have executed since the last .commit() or .rollback()).

        Furthermore, it is an error to call .commit() or .rollback() within the
        TPC transaction. A ProgrammingError is raised, if the application calls
        .commit() or .rollback() during an active TPC transaction.

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        self._xid = xid
        if self.autocommit:
            self.execute(self._cursor, "begin transaction", None)

    def tpc_prepare(self):
        """Performs the first phase of a transaction started with .tpc_begin().
        A ProgrammingError is be raised if this method is called outside of a
        TPC transaction.

        After calling .tpc_prepare(), no statements can be executed until
        .tpc_commit() or .tpc_rollback() have been called.

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        q = "PREPARE TRANSACTION '%s';" % (self._xid[1],)
        self.execute(self._cursor, q, None)

    def tpc_commit(self, xid=None):
        """When called with no arguments, .tpc_commit() commits a TPC
        transaction previously prepared with .tpc_prepare().

        If .tpc_commit() is called prior to .tpc_prepare(), a single phase
        commit is performed. A transaction manager may choose to do this if
        only a single resource is participating in the global transaction.

        When called with a transaction ID xid, the database commits the given
        transaction. If an invalid transaction ID is provided, a
        ProgrammingError will be raised. This form should be called outside of
        a transaction, and is intended for use in recovery.

        On return, the TPC transaction is ended.

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        if xid is None:
            xid = self._xid

        if xid is None:
            raise ProgrammingError(
                "Cannot tpc_commit() without a TPC transaction!")

        try:
            previous_autocommit_mode = self.autocommit
            self.autocommit = True
            if xid in self.tpc_recover():
                self.execute(
                    self._cursor, "COMMIT PREPARED '%s';" % (xid[1], ),
                    None)
            else:
                # a single-phase commit
                self.commit()
        finally:
            self.autocommit = previous_autocommit_mode
        self._xid = None

    def tpc_rollback(self, xid=None):
        """When called with no arguments, .tpc_rollback() rolls back a TPC
        transaction. It may be called before or after .tpc_prepare().

        When called with a transaction ID xid, it rolls back the given
        transaction. If an invalid transaction ID is provided, a
        ProgrammingError is raised. This form should be called outside of a
        transaction, and is intended for use in recovery.

        On return, the TPC transaction is ended.

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        if xid is None:
            xid = self._xid

        if xid is None:
            raise ProgrammingError(
                "Cannot tpc_rollback() without a TPC prepared transaction!")

        try:
            previous_autocommit_mode = self.autocommit
            self.autocommit = True
            if xid in self.tpc_recover():
                # a two-phase rollback
                self.execute(
                    self._cursor, "ROLLBACK PREPARED '%s';" % (xid[1],),
                    None)
            else:
                # a single-phase rollback
                self.rollback()
        finally:
            self.autocommit = previous_autocommit_mode
        self._xid = None

    def tpc_recover(self):
        """Returns a list of pending transaction IDs suitable for use with
        .tpc_commit(xid) or .tpc_rollback(xid).

        This function is part of the `DBAPI 2.0 specification
        <http://www.python.org/dev/peps/pep-0249/>`_.
        """
        try:
            previous_autocommit_mode = self.autocommit
            self.autocommit = True
            curs = self.cursor()
            curs.execute("select gid FROM pg_prepared_xacts")
            return [self.xid(0, row[0], '') for row in curs]
        finally:
            self.autocommit = previous_autocommit_mode

# pg element oid -> pg array typeoid
pg_array_types = {
    16: 1000,
    25: 1009,    # TEXT[]
    701: 1022,
    1700: 1231,  # NUMERIC[]
}


# PostgreSQL encodings:
#   http://www.postgresql.org/docs/8.3/interactive/multibyte.html
# Python encodings:
#   http://www.python.org/doc/2.4/lib/standard-encodings.html
#
# Commented out encodings don't require a name change between PostgreSQL and
# Python.  If the py side is None, then the encoding isn't supported.
pg_to_py_encodings = {
    # Not supported:
    "mule_internal": None,
    "euc_tw": None,

    # Name fine as-is:
    # "euc_jp",
    # "euc_jis_2004",
    # "euc_kr",
    # "gb18030",
    # "gbk",
    # "johab",
    # "sjis",
    # "shift_jis_2004",
    # "uhc",
    # "utf8",

    # Different name:
    "euc_cn": "gb2312",
    "iso_8859_5": "is8859_5",
    "iso_8859_6": "is8859_6",
    "iso_8859_7": "is8859_7",
    "iso_8859_8": "is8859_8",
    "koi8": "koi8_r",
    "latin1": "iso8859-1",
    "latin2": "iso8859_2",
    "latin3": "iso8859_3",
    "latin4": "iso8859_4",
    "latin5": "iso8859_9",
    "latin6": "iso8859_10",
    "latin7": "iso8859_13",
    "latin8": "iso8859_14",
    "latin9": "iso8859_15",
    "sql_ascii": "ascii",
    "win866": "cp886",
    "win874": "cp874",
    "win1250": "cp1250",
    "win1251": "cp1251",
    "win1252": "cp1252",
    "win1253": "cp1253",
    "win1254": "cp1254",
    "win1255": "cp1255",
    "win1256": "cp1256",
    "win1257": "cp1257",
    "win1258": "cp1258",
    "unicode": "utf-8",  # Needed for Amazon Redshift
}


def walk_array(arr):
    for i, v in enumerate(arr):
        if isinstance(v, list):
            for a, i2, v2 in walk_array(v):
                yield a, i2, v2
        else:
            yield arr, i, v


def array_find_first_element(arr):
    for v in array_flatten(arr):
        if v is not None:
            return v
    return None


def array_flatten(arr):
    for v in arr:
        if isinstance(v, list):
            for v2 in array_flatten(v):
                yield v2
        else:
            yield v


def array_check_dimensions(arr):
    v0 = arr[0]
    if isinstance(v0, list):
        req_len = len(v0)
        req_inner_lengths = array_check_dimensions(v0)
        for v in arr:
            inner_lengths = array_check_dimensions(v)
            if len(v) != req_len or inner_lengths != req_inner_lengths:
                raise ArrayDimensionsNotConsistentError(
                    "array dimensions not consistent")
        retval = [req_len]
        retval.extend(req_inner_lengths)
        return retval
    else:
        # make sure nothing else at this level is a list
        for v in arr:
            if isinstance(v, list):
                raise ArrayDimensionsNotConsistentError(
                    "array dimensions not consistent")
        return []


def array_has_null(arr):
    for v in array_flatten(arr):
        if v is None:
            return True
    return False


def array_dim_lengths(arr):
    v0 = arr[0]
    if isinstance(v0, list):
        retval = [len(v0)]
        retval.extend(array_dim_lengths(v0))
    else:
        return [len(arr)]
    return retval
