# Python implementation of the MySQL client-server protocol
#   http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol

try:
    import hashlib
    sha_new = lambda *args, **kwargs: hashlib.new("sha1", *args, **kwargs)
except ImportError:
    import sha
    sha_new = sha.new

import socket
try:
    import ssl
    SSL_ENABLED = True
except ImportError:
    SSL_ENABLED = False

import struct
import sys
import os
import ConfigParser

try:
    import cStringIO as StringIO
except ImportError:
    import StringIO

try:
    import getpass
    DEFAULT_USER = getpass.getuser()
except ImportError:
    DEFAULT_USER = None

from charset import MBLENGTH, charset_by_name, charset_by_id
from cursors import Cursor
from constants import FIELD_TYPE, FLAG
from constants import SERVER_STATUS
from constants.CLIENT import *
from constants.COMMAND import *
from util import join_bytes, byte2int, int2byte
from converters import escape_item, encoders, decoders
from err import raise_mysql_exception, Warning, Error, \
     InterfaceError, DataError, DatabaseError, OperationalError, \
     IntegrityError, InternalError, NotSupportedError, ProgrammingError

DEBUG = False

NULL_COLUMN = 251
UNSIGNED_CHAR_COLUMN = 251
UNSIGNED_SHORT_COLUMN = 252
UNSIGNED_INT24_COLUMN = 253
UNSIGNED_INT64_COLUMN = 254
UNSIGNED_CHAR_LENGTH = 1
UNSIGNED_SHORT_LENGTH = 2
UNSIGNED_INT24_LENGTH = 3
UNSIGNED_INT64_LENGTH = 8

DEFAULT_CHARSET = 'latin1'


def dump_packet(data):
    
    def is_ascii(data):
        if byte2int(data) >= 65 and byte2int(data) <= 122: #data.isalnum():
            return data
        return '.'
    
    try:
        print "packet length %d" % len(data)
        print "method call[1]: %s" % sys._getframe(1).f_code.co_name
        print "method call[2]: %s" % sys._getframe(2).f_code.co_name
        print "method call[3]: %s" % sys._getframe(3).f_code.co_name
        print "method call[4]: %s" % sys._getframe(4).f_code.co_name
        print "method call[5]: %s" % sys._getframe(5).f_code.co_name
        print "-" * 88
    except ValueError: pass
    dump_data = [data[i:i+16] for i in xrange(len(data)) if i%16 == 0]
    for d in dump_data:
        print ' '.join(map(lambda x:"%02X" % byte2int(x), d)) + \
                '   ' * (16 - len(d)) + ' ' * 2 + \
                ' '.join(map(lambda x:"%s" % is_ascii(x), d))
    print "-" * 88
    print ""

def _scramble(password, message):
    if password == None or len(password) == 0:
        return int2byte(0)
    if DEBUG: print 'password=' + password
    stage1 = sha_new(password).digest()
    stage2 = sha_new(stage1).digest()
    s = sha_new()
    s.update(message)
    s.update(stage2)
    result = s.digest()
    return _my_crypt(result, stage1)

def _my_crypt(message1, message2):
    length = len(message1)
    result = struct.pack('B', length)
    for i in xrange(length):
        x = (struct.unpack('B', message1[i:i+1])[0] ^ \
             struct.unpack('B', message2[i:i+1])[0])
        result += struct.pack('B', x)
    return result

# old_passwords support ported from libmysql/password.c
SCRAMBLE_LENGTH_323 = 8

class RandStruct_323(object):
    def __init__(self, seed1, seed2):
        self.max_value = 0x3FFFFFFFL
        self.seed1 = seed1 % self.max_value
        self.seed2 = seed2 % self.max_value

    def my_rnd(self):
        self.seed1 = (self.seed1 * 3L + self.seed2) % self.max_value
        self.seed2 = (self.seed1 + self.seed2 + 33L) % self.max_value
        return float(self.seed1) / float(self.max_value)

def _scramble_323(password, message):
    hash_pass = _hash_password_323(password)
    hash_message = _hash_password_323(message[:SCRAMBLE_LENGTH_323])
    hash_pass_n = struct.unpack(">LL", hash_pass)
    hash_message_n = struct.unpack(">LL", hash_message)

    rand_st = RandStruct_323(hash_pass_n[0] ^ hash_message_n[0],
                             hash_pass_n[1] ^ hash_message_n[1])
    outbuf = StringIO.StringIO()
    for _ in xrange(min(SCRAMBLE_LENGTH_323, len(message))):
        outbuf.write(int2byte(int(rand_st.my_rnd() * 31) + 64))
    extra = int2byte(int(rand_st.my_rnd() * 31))
    out = outbuf.getvalue()
    outbuf = StringIO.StringIO()
    for c in out:
        outbuf.write(int2byte(byte2int(c) ^ byte2int(extra)))
    return outbuf.getvalue()

def _hash_password_323(password):
    nr = 1345345333L
    add = 7L
    nr2 = 0x12345671L

    for c in [byte2int(x) for x in password if x not in (' ', '\t')]:
        nr^= (((nr & 63)+add)*c)+ (nr << 8) & 0xFFFFFFFF
        nr2= (nr2 + ((nr2 << 8) ^ nr)) & 0xFFFFFFFF
        add= (add + c) & 0xFFFFFFFF

    r1 = nr & ((1L << 31) - 1L) # kill sign bits
    r2 = nr2 & ((1L << 31) - 1L)

    # pack
    return struct.pack(">LL", r1, r2)

def pack_int24(n):
    return struct.pack('BBB', n&0xFF, (n>>8)&0xFF, (n>>16)&0xFF)

def unpack_uint16(n):
  return struct.unpack('<H', n[0:2])[0]


# TODO: stop using bit-shifting in these functions...
# TODO: rename to "uint" to make it clear they're unsigned...
def unpack_int24(n):
    try:
        return struct.unpack('B',n[0])[0] + (struct.unpack('B', n[1])[0] << 8) +\
            (struct.unpack('B',n[2])[0] << 16)
    except TypeError:
        return n[0] + (n[1] << 8) + (n[2] << 16)

def unpack_int32(n):
    try:
        return struct.unpack('B',n[0])[0] + (struct.unpack('B', n[1])[0] << 8) +\
            (struct.unpack('B',n[2])[0] << 16) + (struct.unpack('B', n[3])[0] << 24)
    except TypeError:
        return n[0] + (n[1] << 8) + (n[2] << 16) + (n[3] << 24)

def unpack_int64(n):
    try:
        return struct.unpack('B',n[0])[0] + (struct.unpack('B', n[1])[0]<<8) +\
        (struct.unpack('B',n[2])[0] << 16) + (struct.unpack('B',n[3])[0]<<24)+\
        (struct.unpack('B',n[4])[0] << 32) + (struct.unpack('B',n[5])[0]<<40)+\
        (struct.unpack('B',n[6])[0] << 48) + (struct.unpack('B',n[7])[0]<<56)
    except TypeError:
        return n[0] + (n[1] << 8) + (n[2] << 16) + (n[3] << 24) +\
        (n[4] << 32) + (n[5] << 40) + (n[6] << 48) + (n[7] << 56)

def defaulterrorhandler(connection, cursor, errorclass, errorvalue):
    err = errorclass, errorvalue
    if DEBUG:
        raise

    if cursor:
        cursor.messages.append(err)
    else:
        connection.messages.append(err)
    del cursor
    del connection

    if not issubclass(errorclass, Error):
        raise Error(errorclass, errorvalue)
    else:
        raise errorclass, errorvalue


class MysqlPacket(object):
  """Representation of a MySQL response packet.  Reads in the packet
  from the network socket, removes packet header and provides an interface
  for reading/parsing the packet results."""

  def __init__(self, connection):
    self.connection = connection
    self.__position = 0
    self.__recv_packet()

  def __recv_packet(self):
    """Parse the packet header and read entire packet payload into buffer."""
    packet_header = self.connection.rfile.read(4)
    if len(packet_header) < 4:
        raise OperationalError(2013, "Lost connection to MySQL server during query")

    if DEBUG: dump_packet(packet_header)
    packet_length_bin = packet_header[:3]
    self.__packet_number = byte2int(packet_header[3])
    # TODO: check packet_num is correct (+1 from last packet)

    bin_length = packet_length_bin + int2byte(0)  # pad little-endian number
    bytes_to_read = struct.unpack('<I', bin_length)[0]
    recv_data = self.connection.rfile.read(bytes_to_read)
    if len(recv_data) < bytes_to_read:
        raise OperationalError(2013, "Lost connection to MySQL server during query")
    if DEBUG: dump_packet(recv_data)
    self.__data = recv_data

  def packet_number(self): return self.__packet_number

  def get_all_data(self): return self.__data

  def read(self, size):
    """Read the first 'size' bytes in packet and advance cursor past them."""
    result = self.peek(size)
    self.advance(size)
    return result

  def read_all(self):
    """Read all remaining data in the packet.

    (Subsequent read() or peek() will return errors.)
    """
    result = self.__data[self.__position:]
    self.__position = None  # ensure no subsequent read() or peek()
    return result

  def advance(self, length):
    """Advance the cursor in data buffer 'length' bytes."""
    new_position = self.__position + length
    if new_position < 0 or new_position > len(self.__data):
      raise Exception('Invalid advance amount (%s) for cursor.  '
                      'Position=%s' % (length, new_position))
    self.__position = new_position

  def rewind(self, position=0):
    """Set the position of the data buffer cursor to 'position'."""
    if position < 0 or position > len(self.__data):
      raise Exception("Invalid position to rewind cursor to: %s." % position)
    self.__position = position

  def peek(self, size):
    """Look at the first 'size' bytes in packet without moving cursor."""
    result = self.__data[self.__position:(self.__position+size)]
    if len(result) != size:
      error = ('Result length not requested length:\n'
               'Expected=%s.  Actual=%s.  Position: %s.  Data Length: %s'
               % (size, len(result), self.__position, len(self.__data)))
      if DEBUG:
        print error
        self.dump()
      raise AssertionError(error)
    return result

  def get_bytes(self, position, length=1):
    """Get 'length' bytes starting at 'position'.

    Position is start of payload (first four packet header bytes are not
    included) starting at index '0'.

    No error checking is done.  If requesting outside end of buffer
    an empty string (or string shorter than 'length') may be returned!
    """
    return self.__data[position:(position+length)]

  def read_length_coded_binary(self):
    """Read a 'Length Coded Binary' number from the data buffer.

    Length coded numbers can be anywhere from 1 to 9 bytes depending
    on the value of the first byte.
    """
    c = byte2int(self.read(1))
    if c == NULL_COLUMN:
      return None
    if c < UNSIGNED_CHAR_COLUMN:
      return c
    elif c == UNSIGNED_SHORT_COLUMN:
      return unpack_uint16(self.read(UNSIGNED_SHORT_LENGTH))
    elif c == UNSIGNED_INT24_COLUMN:
      return unpack_int24(self.read(UNSIGNED_INT24_LENGTH))
    elif c == UNSIGNED_INT64_COLUMN:
      # TODO: what was 'longlong'?  confirm it wasn't used?
      return unpack_int64(self.read(UNSIGNED_INT64_LENGTH))

  def read_length_coded_string(self):
    """Read a 'Length Coded String' from the data buffer.

    A 'Length Coded String' consists first of a length coded
    (unsigned, positive) integer represented in 1-9 bytes followed by
    that many bytes of binary data.  (For example "cat" would be "3cat".)
    """
    length = self.read_length_coded_binary()
    if length is None:
        return None
    return self.read(length)

  def is_ok_packet(self):
    return byte2int(self.get_bytes(0)) == 0

  def is_eof_packet(self):
    return byte2int(self.get_bytes(0)) == 254  # 'fe'

  def is_resultset_packet(self):
    field_count = byte2int(self.get_bytes(0))
    return field_count >= 1 and field_count <= 250

  def is_error_packet(self):
    return byte2int(self.get_bytes(0)) == 255

  def check_error(self):
    if self.is_error_packet():
      self.rewind()
      self.advance(1)  # field_count == error (we already know that)
      errno = unpack_uint16(self.read(2))
      if DEBUG: print "errno = %d" % errno
      raise_mysql_exception(self.__data)

  def dump(self):
    dump_packet(self.__data)


class FieldDescriptorPacket(MysqlPacket):
  """A MysqlPacket that represents a specific column's metadata in the result.

  Parsing is automatically done and the results are exported via public
  attributes on the class such as: db, table_name, name, length, type_code.
  """

  def __init__(self, *args):
    MysqlPacket.__init__(self, *args)
    self.__parse_field_descriptor()

  def __parse_field_descriptor(self):
    """Parse the 'Field Descriptor' (Metadata) packet.

    This is compatible with MySQL 4.1+ (not compatible with MySQL 4.0).
    """
    self.catalog = self.read_length_coded_string()
    self.db = self.read_length_coded_string()
    self.table_name = self.read_length_coded_string()
    self.org_table = self.read_length_coded_string()
    self.name = self.read_length_coded_string().decode(self.connection.charset)
    self.org_name = self.read_length_coded_string()
    self.advance(1)  # non-null filler
    self.charsetnr = struct.unpack('<H', self.read(2))[0]
    self.length = struct.unpack('<I', self.read(4))[0]
    self.type_code = byte2int(self.read(1))
    self.flags = struct.unpack('<H', self.read(2))[0]
    self.scale = byte2int(self.read(1))  # "decimals"
    self.advance(2)  # filler (always 0x00)

    # 'default' is a length coded binary and is still in the buffer?
    # not used for normal result sets...

  def description(self):
    """Provides a 7-item tuple compatible with the Python PEP249 DB Spec."""
    desc = []
    desc.append(self.name)
    desc.append(self.type_code)
    desc.append(None) # TODO: display_length; should this be self.length?
    desc.append(self.get_column_length()) # 'internal_size'
    desc.append(self.get_column_length()) # 'precision'  # TODO: why!?!?
    desc.append(self.scale)

    # 'null_ok' -- can this be True/False rather than 1/0?
    #              if so just do:  desc.append(bool(self.flags % 2 == 0))
    if self.flags % 2 == 0:
      desc.append(1)
    else:
      desc.append(0)
    return tuple(desc)

  def get_column_length(self):
    if self.type_code == FIELD_TYPE.VAR_STRING:
      mblen = MBLENGTH.get(self.charsetnr, 1)
      return self.length // mblen
    return self.length

  def __str__(self):
    return ('%s %s.%s.%s, type=%s'
            % (self.__class__, self.db, self.table_name, self.name,
               self.type_code))

class OKPacketWrapper(object):
    """
    OK Packet Wrapper. It uses an existing packet object, and wraps
    around it, exposing useful variables while still providing access
    to the original packet objects variables and methods.
    """

    def __init__(self, from_packet):
        if not from_packet.is_ok_packet():
            raise ValueError('Cannot create ' + str(self.__class__.__name__)
                + ' object from invalid packet type')
        
        self.packet = from_packet
        self.packet.advance(1)
        
        self.affected_rows = self.packet.read_length_coded_binary()
        self.insert_id = self.packet.read_length_coded_binary()
        self.server_status = struct.unpack('<H', self.packet.read(2))[0]
        self.warning_count = struct.unpack('<H', self.packet.read(2))[0]
        self.message = self.packet.read_all()
    
    def __getattr__(self, key):
        if hasattr(self.packet, key):
            return getattr(self.packet, key)
        
        raise AttributeError(str(self.__class__)
            + " instance has no attribute '" + key + "'")

class EOFPacketWrapper(object):
    """
    EOF Packet Wrapper. It uses an existing packet object, and wraps
    around it, exposing useful variables while still providing access
    to the original packet objects variables and methods.
    """

    def __init__(self, from_packet):
        if not from_packet.is_eof_packet():
            raise ValueError('Cannot create ' + str(self.__class__.__name__)
                + ' object from invalid packet type')
        
        self.packet = from_packet
        self.warning_count = self.packet.read(2)
        server_status = struct.unpack('<h', self.packet.read(2))[0]
        self.has_next = (server_status
                        & SERVER_STATUS.SERVER_MORE_RESULTS_EXISTS)

    def __getattr__(self, key):
        if hasattr(self.packet, key):
            return getattr(self.packet, key)
        
        raise AttributeError(str(self.__class__)
            + " instance has no attribute '" + key + "'")

class Connection(object):
    """
    Representation of a socket with a mysql server.

    The proper way to get an instance of this class is to call
    connect()."""
    errorhandler = defaulterrorhandler

    def __init__(self, host="localhost", user=None, passwd="",
                 db=None, port=3306, unix_socket=None,
                 charset='', sql_mode=None,
                 read_default_file=None, conv=decoders, use_unicode=None,
                 client_flag=0, cursorclass=Cursor, init_command=None,
                 connect_timeout=None, ssl=None, read_default_group=None,
                 compress=None, named_pipe=None):
        """
        Establish a connection to the MySQL database. Accepts several
        arguments:

        host: Host where the database server is located
        user: Username to log in as
        passwd: Password to use.
        db: Database to use, None to not use a particular one.
        port: MySQL port to use, default is usually OK.
        unix_socket: Optionally, you can use a unix socket rather than TCP/IP.
        charset: Charset you want to use.
        sql_mode: Default SQL_MODE to use.
        read_default_file: Specifies  my.cnf file to read these parameters from under the [client] section.
        conv: Decoders dictionary to use instead of the default one. This is used to provide custom marshalling of types. See converters.
        use_unicode: Whether or not to default to unicode strings. This option defaults to true for Py3k.
        client_flag: Custom flags to send to MySQL. Find potential values in constants.CLIENT.
        cursorclass: Custom cursor class to use.
        init_command: Initial SQL statement to run when connection is established.
        connect_timeout: Timeout before throwing an exception when connecting.
        ssl: A dict of arguments similar to mysql_ssl_set()'s parameters. For now the capath and cipher arguments are not supported.
        read_default_group: Group to read from in the configuration file.
        compress; Not supported
        named_pipe: Not supported
        """

        if use_unicode is None and sys.version_info[0] > 2:
            use_unicode = True

        if compress or named_pipe:
            raise NotImplementedError, "compress and named_pipe arguments are not supported"

        if ssl and (ssl.has_key('capath') or ssl.has_key('cipher')):
            raise NotImplementedError, 'ssl options capath and cipher are not supported'

        self.ssl = False
        if ssl:
            if not SSL_ENABLED:
                raise NotImplementedError, "ssl module not found"
            self.ssl = True
            client_flag |= SSL
            for k in ('key', 'cert', 'ca'):
                v = None
                if ssl.has_key(k):
                    v = ssl[k]
                setattr(self, k, v)

        if read_default_group and not read_default_file:
            if sys.platform.startswith("win"):
                read_default_file = "c:\\my.ini"
            else:
                read_default_file = "/etc/my.cnf"

        if read_default_file:
            if not read_default_group:
                read_default_group = "client"

            cfg = ConfigParser.RawConfigParser()
            cfg.read(os.path.expanduser(read_default_file))

            def _config(key, default):
                try:
                    return cfg.get(read_default_group,key)
                except:
                    return default

            user = _config("user",user)
            passwd = _config("password",passwd)
            host = _config("host", host)
            db = _config("db",db)
            unix_socket = _config("socket",unix_socket)
            port = int(_config("port", port))
            charset = _config("default-character-set", charset)

        self.host = host
        self.port = port
        self.user = user or DEFAULT_USER
        self.password = passwd
        self.db = db
        self.unix_socket = unix_socket
        if charset:
            self.charset = charset
            self.use_unicode = True
        else:
            self.charset = DEFAULT_CHARSET
            self.use_unicode = False

        if use_unicode is not None:
            self.use_unicode = use_unicode

        client_flag |= CAPABILITIES
        client_flag |= MULTI_STATEMENTS
        if self.db:
            client_flag |= CONNECT_WITH_DB
        self.client_flag = client_flag

        self.cursorclass = cursorclass
        self.connect_timeout = connect_timeout

        self._connect()

        self._result = None
        self._affected_rows = 0
        self.host_info = "Not connected"

        self.messages = []
        self.set_charset(charset)
        self.encoders = encoders
        self.decoders = conv

        self.autocommit(False)

        if sql_mode is not None:
            c = self.cursor()
            c.execute("SET sql_mode=%s", (sql_mode,))

        self.commit()

        if init_command is not None:
            c = self.cursor()
            c.execute(init_command)

            self.commit()


    def close(self):
        ''' Send the quit message and close the socket '''
        if self.socket is None:
            raise Error("Already closed")
        send_data = struct.pack('<i',1) + int2byte(COM_QUIT)
        self.wfile.write(send_data)
        self.wfile.close()
        self.rfile.close()
        self.socket.close()
        self.socket = None
        self.rfile = None
        self.wfile = None

    def autocommit(self, value):
        ''' Set whether or not to commit after every execute() '''
        try:
            self._execute_command(COM_QUERY, "SET AUTOCOMMIT = %s" % \
                                      self.escape(value))
            self.read_packet()
        except:
            exc,value,tb = sys.exc_info()
            self.errorhandler(None, exc, value)

    def commit(self):
        ''' Commit changes to stable storage '''
        try:
            self._execute_command(COM_QUERY, "COMMIT")
            self.read_packet()
        except:
            exc,value,tb = sys.exc_info()
            self.errorhandler(None, exc, value)

    def rollback(self):
        ''' Roll back the current transaction '''
        try:
            self._execute_command(COM_QUERY, "ROLLBACK")
            self.read_packet()
        except:
            exc,value,tb = sys.exc_info()
            self.errorhandler(None, exc, value)

    def escape(self, obj):
        ''' Escape whatever value you pass to it  '''
        return escape_item(obj, self.charset)

    def literal(self, obj):
        ''' Alias for escape() '''
        return escape_item(obj, self.charset)

    def cursor(self, cursor=None):
        ''' Create a new cursor to execute queries with '''
        if cursor:
            return cursor(self)
        return self.cursorclass(self)

    def __enter__(self):
        ''' Context manager that returns a Cursor '''
        return self.cursor()

    def __exit__(self, exc, value, traceback):
        ''' On successful exit, commit. On exception, rollback. '''
        if exc:
            self.rollback()
        else:
            self.commit()

    # The following methods are INTERNAL USE ONLY (called from Cursor)
    def query(self, sql, unbuffered=False):
        if DEBUG:
            print "sending query: %s" % sql
        self._execute_command(COM_QUERY, sql)
        self._affected_rows = self._read_query_result(unbuffered=unbuffered)
        return self._affected_rows

    def next_result(self):
        self._affected_rows = self._read_query_result()
        return self._affected_rows

    def affected_rows(self):
        return self._affected_rows

    def kill(self, thread_id):
        arg = struct.pack('<I', thread_id)
        try:
            self._execute_command(COM_PROCESS_KILL, arg)
        except:
            exc,value,tb = sys.exc_info()
            self.errorhandler(None, exc, value)
            return
        pkt = self.read_packet()
        return pkt.is_ok_packet()

    def ping(self, reconnect=True):
        ''' Check if the server is alive '''
        try:
            self._execute_command(COM_PING, "")
            pkt = self.read_packet()
            return pkt.is_ok_packet()
        except:
            if reconnect:
                self._connect()
                return self.ping(False)
            else:
                exc,value,tb = sys.exc_info()
                self.errorhandler(None, exc, value)
                return

    def set_charset(self, charset):
        try:
            if charset:
                self._execute_command(COM_QUERY, "SET NAMES %s" %
                                      self.escape(charset))
                self.read_packet()
                self.charset = charset
        except:
            exc,value,tb = sys.exc_info()
            self.errorhandler(None, exc, value)

    def _connect(self):
        try:
            if self.unix_socket and (self.host == 'localhost' or self.host == '127.0.0.1'):
                sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                t = sock.gettimeout()
                sock.settimeout(self.connect_timeout)
                sock.connect(self.unix_socket)
                sock.settimeout(t)
                self.host_info = "Localhost via UNIX socket"
                if DEBUG: print 'connected using unix_socket'
            else:
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                t = sock.gettimeout()
                sock.settimeout(self.connect_timeout)
                sock.connect((self.host, self.port))
                sock.settimeout(t)
                self.host_info = "socket %s:%d" % (self.host, self.port)
                if DEBUG: print 'connected using socket'
            self.socket = sock
            self.rfile = self.socket.makefile("rb")
            self.wfile = self.socket.makefile("wb")
            self._get_server_information()
            self._request_authentication()
        except socket.error, e:
            raise OperationalError(2003, "Can't connect to MySQL server on %r (%s)" % (self.host, e.args[0]))

    def read_packet(self, packet_type=MysqlPacket):
      """Read an entire "mysql packet" in its entirety from the network
      and return a MysqlPacket type that represents the results."""

      packet = packet_type(self)
      packet.check_error()
      return packet

    def _read_query_result(self, unbuffered=False):
        if unbuffered:
            try:
                result = MySQLResult(self)
                result.init_unbuffered_query()
            except:
                result.unbuffered_active = False
                raise
        else:
            result = MySQLResult(self)
            result.read()
        self._result = result
        return result.affected_rows

    def insert_id(self):
        if self._result:
            return self._result.insert_id
        else:
            return 0

    def _send_command(self, command, sql):
        #send_data = struct.pack('<i', len(sql) + 1) + command + sql
        # could probably be more efficient, at least it's correct
        if not self.socket:
            self.errorhandler(None, InterfaceError, "(0, '')")

        # If the last query was unbuffered, make sure it finishes before
        # sending new commands
        if self._result is not None and self._result.unbuffered_active:
            self._result._finish_unbuffered_query()

        if isinstance(sql, unicode):
            sql = sql.encode(self.charset)

        prelude = struct.pack('<i', len(sql)+1) + int2byte(command)
        self.wfile.write(prelude + sql)
        self.wfile.flush()
        if DEBUG: dump_packet(prelude + sql)

    def _execute_command(self, command, sql):
        self._send_command(command, sql)
        
    def _request_authentication(self):
        self._send_authentication()

    def _send_authentication(self):
        self.client_flag |= CAPABILITIES
        if self.server_version.startswith('5'):
            self.client_flag |= MULTI_RESULTS

        if self.user is None:
            raise ValueError, "Did not specify a username"

        charset_id = charset_by_name(self.charset).id
        self.user = self.user.encode(self.charset)

        data_init = struct.pack('<i', self.client_flag) + struct.pack("<I", 1) + \
                     int2byte(charset_id) + int2byte(0)*23

        next_packet = 1

        if self.ssl:
            data = pack_int24(len(data_init)) + int2byte(next_packet) + data_init
            next_packet += 1

            if DEBUG: dump_packet(data)

            self.wfile.write(data)
            self.wfile.flush()
            self.socket = ssl.wrap_self.socketet(self.socket, keyfile=self.key,
                                                 certfile=self.cert,
                                                 ssl_version=ssl.PROTOCOL_TLSv1,
                                                 cert_reqs=ssl.CERT_REQUIRED,
                                                 ca_certs=self.ca)
            self.rfile = self.socket.makefile("rb")
            self.wfile = self.socket.makefile("wb")

        data = data_init + self.user+int2byte(0) + _scramble(self.password.encode(self.charset), self.salt)

        if self.db:
            self.db = self.db.encode(self.charset)
            data += self.db + int2byte(0)

        data = pack_int24(len(data)) + int2byte(next_packet) + data
        next_packet += 2

        if DEBUG: dump_packet(data)

        self.wfile.write(data)
        self.wfile.flush()

        auth_packet = MysqlPacket(self)
        auth_packet.check_error()
        if DEBUG: auth_packet.dump()

        # if old_passwords is enabled the packet will be 1 byte long and
        # have the octet 254

        if auth_packet.is_eof_packet():
            # send legacy handshake
            #raise NotImplementedError, "old_passwords are not supported. Check to see if mysqld was started with --old-passwords, if old-passwords=1 in a my.cnf file, or if there are some short hashes in your mysql.user table."
            # TODO: is this the correct charset?
            data = _scramble_323(self.password.encode(self.charset), self.salt.encode(self.charset)) + int2byte(0)
            data = pack_int24(len(data)) + int2byte(next_packet) + data

            self.wfile.write(data)
            self.wfile.flush()
            auth_packet = MysqlPacket(self)
            auth_packet.check_error()
            if DEBUG: auth_packet.dump()


    # _mysql support
    def thread_id(self):
        return self.server_thread_id[0]

    def character_set_name(self):
        return self.charset

    def get_host_info(self):
        return self.host_info

    def get_proto_info(self):
        return self.protocol_version

    def _get_server_information(self):
        i = 0
        packet = MysqlPacket(self)
        data = packet.get_all_data()

        if DEBUG: dump_packet(data)
        #packet_len = byte2int(data[i:i+1])
        #i += 4
        self.protocol_version = byte2int(data[i:i+1])

        i += 1
        server_end = data.find(int2byte(0), i)
        # TODO: is this the correct charset? should it be default_charset?
        self.server_version = data[i:server_end].decode(self.charset)

        i = server_end + 1
        self.server_thread_id = struct.unpack('<h', data[i:i+2])

        i += 4
        self.salt = data[i:i+8]

        i += 9
        if len(data) >= i + 1:
            i += 1

        self.server_capabilities = struct.unpack('<h', data[i:i+2])[0]

        i += 1
        self.server_language = byte2int(data[i:i+1])
        self.server_charset = charset_by_id(self.server_language).name

        i += 16
        if len(data) >= i+12-1:
            rest_salt = data[i:i+12]
            self.salt += rest_salt

    def get_server_info(self):
        return self.server_version

    Warning = Warning
    Error = Error
    InterfaceError = InterfaceError
    DatabaseError = DatabaseError
    DataError = DataError
    OperationalError = OperationalError
    IntegrityError = IntegrityError
    InternalError = InternalError
    ProgrammingError = ProgrammingError
    NotSupportedError = NotSupportedError

# TODO: move OK and EOF packet parsing/logic into a proper subclass
#       of MysqlPacket like has been done with FieldDescriptorPacket.
class MySQLResult(object):

    def __init__(self, connection):
        from weakref import proxy
        self.connection = proxy(connection)
        self.affected_rows = None
        self.insert_id = None
        self.server_status = 0
        self.warning_count = 0
        self.message = None
        self.field_count = 0
        self.description = None
        self.rows = None
        self.has_next = None
        self.unbuffered_active = False

    def __del__(self):
        if self.unbuffered_active:
            self._finish_unbuffered_query()

    def read(self):
        self.first_packet = self.connection.read_packet()

        # TODO: use classes for different packet types?
        if self.first_packet.is_ok_packet():
            self._read_ok_packet()
        else:
            self._read_result_packet()

    def init_unbuffered_query(self):
        self.unbuffered_active = True
        self.first_packet = self.connection.read_packet()

        if self.first_packet.is_ok_packet():
            self._read_ok_packet()
            self.unbuffered_active = False
        else:
            self.field_count = byte2int(self.first_packet.read(1))
            self._get_descriptions()
            
            # Apparently, MySQLdb picks this number because it's the maximum
            # value of a 64bit unsigned integer. Since we're emulating MySQLdb,
            # we set it to this instead of None, which would be preferred.
            self.affected_rows = 18446744073709551615

    def _read_ok_packet(self):
        ok_packet = OKPacketWrapper(self.first_packet)
        self.affected_rows = ok_packet.affected_rows
        self.insert_id = ok_packet.insert_id
        self.server_status = ok_packet.server_status
        self.warning_count = ok_packet.warning_count
        self.message = ok_packet.message

    def _check_packet_is_eof(self, packet):
        if packet.is_eof_packet():
            eof_packet = EOFPacketWrapper(packet)
            self.warning_count = eof_packet.warning_count
            self.has_next = eof_packet.has_next
            return True
        return False

    def _read_result_packet(self):
        self.field_count = byte2int(self.first_packet.read(1))
        self._get_descriptions()
        self._read_rowdata_packet()

    def _read_rowdata_packet_unbuffered(self):
        # Check if in an active query
        if self.unbuffered_active == False: return
        
        # EOF
        packet = self.connection.read_packet()
        if self._check_packet_is_eof(packet):
            self.unbuffered_active = False
            self.rows = None
            return

        row = []
        for field in self.fields:
            data = packet.read_length_coded_string()
            converted = None
            if field.type_code in self.connection.decoders:
                converter = self.connection.decoders[field.type_code]
                if DEBUG: print "DEBUG: field=%s, converter=%s" % (field, converter)
                if data != None:
                    converted = converter(self.connection, field, data)
            row.append(converted)

        self.affected_rows = 1
        self.rows = tuple((row))
        if DEBUG: self.rows

    def _finish_unbuffered_query(self):
        # After much reading on the MySQL protocol, it appears that there is,
        # in fact, no way to stop MySQL from sending all the data after
        # executing a query, so we just spin, and wait for an EOF packet.
        while self.unbuffered_active:
            packet = self.connection.read_packet()
            if self._check_packet_is_eof(packet):
                self.unbuffered_active = False

    # TODO: implement this as an iteratable so that it is more
    #       memory efficient and lower-latency to client...
    def _read_rowdata_packet(self):
      """Read a rowdata packet for each data row in the result set."""
      rows = []
      while True:
        packet = self.connection.read_packet()
        if self._check_packet_is_eof(packet):
            break

        row = []
        for field in self.fields:
            data = packet.read_length_coded_string()
            converted = None
            if field.type_code in self.connection.decoders:
                converter = self.connection.decoders[field.type_code]
                if DEBUG: print "DEBUG: field=%s, converter=%s" % (field, converter)
                if data != None:
                    converted = converter(self.connection, field, data)
            row.append(converted)

        rows.append(tuple(row))

      self.affected_rows = len(rows)
      self.rows = tuple(rows)
      if DEBUG: self.rows

    def _get_descriptions(self):
        """Read a column descriptor packet for each column in the result."""
        self.fields = []
        description = []
        for i in xrange(self.field_count):
            field = self.connection.read_packet(FieldDescriptorPacket)
            self.fields.append(field)
            description.append(field.description())

        eof_packet = self.connection.read_packet()
        assert eof_packet.is_eof_packet(), 'Protocol error, expecting EOF'
        self.description = tuple(description)
