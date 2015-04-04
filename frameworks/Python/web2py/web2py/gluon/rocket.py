# -*- coding: utf-8 -*-

# This file is part of the Rocket Web Server
# Copyright (c) 2011 Timothy Farrell
# Modified by Massimo Di Pierro

# Import System Modules
import sys
import errno
import socket
import logging
import platform

# Define Constants
VERSION = '1.2.6'
SERVER_NAME = socket.gethostname()
SERVER_SOFTWARE = 'Rocket %s' % VERSION
HTTP_SERVER_SOFTWARE = '%s Python/%s' % (
    SERVER_SOFTWARE, sys.version.split(' ')[0])
BUF_SIZE = 16384
SOCKET_TIMEOUT = 10  # in secs
THREAD_STOP_CHECK_INTERVAL = 1  # in secs, How often should threads check for a server stop message?
IS_JYTHON = platform.system() == 'Java'  # Handle special cases for Jython
IGNORE_ERRORS_ON_CLOSE = set([errno.ECONNABORTED, errno.ECONNRESET])
DEFAULT_LISTEN_QUEUE_SIZE = 5
DEFAULT_MIN_THREADS = 10
DEFAULT_MAX_THREADS = 0
DEFAULTS = dict(LISTEN_QUEUE_SIZE=DEFAULT_LISTEN_QUEUE_SIZE,
                MIN_THREADS=DEFAULT_MIN_THREADS,
                MAX_THREADS=DEFAULT_MAX_THREADS)

PY3K = sys.version_info[0] > 2


class NullHandler(logging.Handler):
    """A Logging handler to prevent library errors."""
    def emit(self, record):
        pass

if PY3K:
    def b(val):
        """ Convert string/unicode/bytes literals into bytes.  This allows for
        the same code to run on Python 2.x and 3.x. """
        if isinstance(val, str):
            return val.encode()
        else:
            return val

    def u(val, encoding="us-ascii"):
        """ Convert bytes into string/unicode.  This allows for the
        same code to run on Python 2.x and 3.x. """
        if isinstance(val, bytes):
            return val.decode(encoding)
        else:
            return val

else:
    def b(val):
        """ Convert string/unicode/bytes literals into bytes.  This allows for
        the same code to run on Python 2.x and 3.x. """
        if isinstance(val, unicode):
            return val.encode()
        else:
            return val

    def u(val, encoding="us-ascii"):
        """ Convert bytes into string/unicode.  This allows for the
        same code to run on Python 2.x and 3.x. """
        if isinstance(val, str):
            return val.decode(encoding)
        else:
            return val

# Import Package Modules
# package imports removed in monolithic build

__all__ = ['VERSION', 'SERVER_SOFTWARE', 'HTTP_SERVER_SOFTWARE', 'BUF_SIZE',
           'IS_JYTHON', 'IGNORE_ERRORS_ON_CLOSE', 'DEFAULTS', 'PY3K', 'b', 'u',
           'Rocket', 'CherryPyWSGIServer', 'SERVER_NAME', 'NullHandler']

# Monolithic build...end of module: rocket/__init__.py
# Monolithic build...start of module: rocket/connection.py

# Import System Modules
import sys
import time
import socket
try:
    import ssl
    has_ssl = True
except ImportError:
    has_ssl = False
# Import Package Modules
# package imports removed in monolithic build
# TODO - This part is still very experimental.
# from .filelike import FileLikeSocket


class Connection(object):
    __slots__ = [
        'setblocking',
        'sendall',
        'shutdown',
        'makefile',
        'fileno',
        'client_addr',
        'client_port',
        'server_port',
        'socket',
        'start_time',
        'ssl',
        'secure',
        'recv',
        'send',
        'read',
        'write'
    ]

    def __init__(self, sock_tuple, port, secure=False):
        self.client_addr, self.client_port = sock_tuple[1][:2]
        self.server_port = port
        self.socket = sock_tuple[0]
        self.start_time = time.time()
        self.ssl = has_ssl and isinstance(self.socket, ssl.SSLSocket)
        self.secure = secure

        if IS_JYTHON:
            # In Jython we must set TCP_NODELAY here since it does not
            # inherit from the listening socket.
            # See: http://bugs.jython.org/issue1309
            self.socket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)

        self.socket.settimeout(SOCKET_TIMEOUT)

        self.shutdown = self.socket.shutdown
        self.fileno = self.socket.fileno
        self.setblocking = self.socket.setblocking
        self.recv = self.socket.recv
        self.send = self.socket.send
        self.makefile = self.socket.makefile

        if sys.platform == 'darwin':
            self.sendall = self._sendall_darwin
        else:
            self.sendall = self.socket.sendall

    def _sendall_darwin(self, buf):
        pending = len(buf)
        offset = 0
        while pending:
            try:
                sent = self.socket.send(buf[offset:])
                pending -= sent
                offset += sent
            except socket.error:
                import errno
                info = sys.exc_info()
                if info[1].args[0] != errno.EAGAIN:
                    raise
        return offset

# FIXME - this is not ready for prime-time yet.
#    def makefile(self, buf_size=BUF_SIZE):
#        return FileLikeSocket(self, buf_size)

    def close(self):
        if hasattr(self.socket, '_sock'):
            try:
                self.socket._sock.close()
            except socket.error:
                info = sys.exc_info()
                if info[1].args[0] != socket.EBADF:
                    raise info[1]
                else:
                    pass
        self.socket.close()

# Monolithic build...end of module: rocket/connection.py
# Monolithic build...start of module: rocket/filelike.py

# Import System Modules
import socket
try:
    from io import StringIO
except ImportError:
    try:
        from cStringIO import StringIO
    except ImportError:
        from StringIO import StringIO
# Import Package Modules
# package imports removed in monolithic build


class FileLikeSocket(object):
    def __init__(self, conn, buf_size=BUF_SIZE):
        self.conn = conn
        self.buf_size = buf_size
        self.buffer = StringIO()
        self.content_length = None

        if self.conn.socket.gettimeout() == 0.0:
            self.read = self.non_blocking_read
        else:
            self.read = self.blocking_read

    def __iter__(self):
        return self

    def recv(self, size):
        while True:
            try:
                return self.conn.recv(size)
            except socket.error:
                exc = sys.exc_info()
                e = exc[1]
                # FIXME - Don't raise socket_errors_nonblocking or socket_error_eintr
                if (e.args[0] not in set()):
                    raise

    def next(self):
        data = self.readline()
        if data == '':
            raise StopIteration
        return data

    def non_blocking_read(self, size=None):
        # Shamelessly adapted from Cherrypy!
        bufr = self.buffer
        bufr.seek(0, 2)
        if size is None:
            while True:
                data = self.recv(self.buf_size)
                if not data:
                    break
                bufr.write(data)

            self.buffer = StringIO()

            return bufr.getvalue()
        else:
            buf_len = self.buffer.tell()
            if buf_len >= size:
                bufr.seek(0)
                data = bufr.read(size)
                self.buffer = StringIO(bufr.read())
                return data

            self.buffer = StringIO()
            while True:
                remaining = size - buf_len
                data = self.recv(remaining)

                if not data:
                    break

                n = len(data)
                if n == size and not buf_len:
                    return data

                if n == remaining:
                    bufr.write(data)
                    del data
                    break

                bufr.write(data)
                buf_len += n
                del data

            return bufr.getvalue()

    def blocking_read(self, length=None):
        if length is None:
            if self.content_length is not None:
                length = self.content_length
            else:
                length = 1

        try:
            data = self.conn.recv(length)
        except:
            data = b('')

        return data

    def readline(self):
        data = b("")
        char = self.read(1)
        while char != b('\n') and char is not b(''):
            line = repr(char)
            data += char
            char = self.read(1)
        data += char
        return data

    def readlines(self, hint="ignored"):
        return list(self)

    def close(self):
        self.conn = None
        self.content_length = None

# Monolithic build...end of module: rocket/filelike.py
# Monolithic build...start of module: rocket/futures.py

# Import System Modules
import time
try:
    from concurrent.futures import Future, ThreadPoolExecutor
    from concurrent.futures.thread import _WorkItem
    has_futures = True
except ImportError:
    has_futures = False

    class Future:
        pass

    class ThreadPoolExecutor:
        pass

    class _WorkItem:
        pass


class WSGIFuture(Future):
    def __init__(self, f_dict, *args, **kwargs):
        Future.__init__(self, *args, **kwargs)

        self.timeout = None

        self._mem_dict = f_dict
        self._lifespan = 30
        self._name = None
        self._start_time = time.time()

    def set_running_or_notify_cancel(self):
        if time.time() - self._start_time >= self._lifespan:
            self.cancel()
        else:
            return super(WSGIFuture, self).set_running_or_notify_cancel()

    def remember(self, name, lifespan=None):
        self._lifespan = lifespan or self._lifespan

        if name in self._mem_dict:
            raise NameError('Cannot remember future by name "%s".  ' % name +
                            'A future already exists with that name.')
        self._name = name
        self._mem_dict[name] = self

        return self

    def forget(self):
        if self._name in self._mem_dict and self._mem_dict[self._name] is self:
            del self._mem_dict[self._name]
            self._name = None


class _WorkItem(object):
    def __init__(self, future, fn, args, kwargs):
        self.future = future
        self.fn = fn
        self.args = args
        self.kwargs = kwargs

    def run(self):
        if not self.future.set_running_or_notify_cancel():
            return

        try:
            result = self.fn(*self.args, **self.kwargs)
        except BaseException:
            e = sys.exc_info()[1]
            self.future.set_exception(e)
        else:
            self.future.set_result(result)


class WSGIExecutor(ThreadPoolExecutor):
    multithread = True
    multiprocess = False

    def __init__(self, *args, **kwargs):
        ThreadPoolExecutor.__init__(self, *args, **kwargs)

        self.futures = dict()

    def submit(self, fn, *args, **kwargs):
        if self._shutdown_lock.acquire():
            if self._shutdown:
                self._shutdown_lock.release()
                raise RuntimeError(
                    'Cannot schedule new futures after shutdown')

            f = WSGIFuture(self.futures)
            w = _WorkItem(f, fn, args, kwargs)

            self._work_queue.put(w)
            self._adjust_thread_count()
            self._shutdown_lock.release()
            return f
        else:
            return False


class FuturesMiddleware(object):
    """Futures middleware that adds a Futures Executor to the environment"""
    def __init__(self, app, threads=5):
        self.app = app
        self.executor = WSGIExecutor(threads)

    def __call__(self, environ, start_response):
        environ["wsgiorg.executor"] = self.executor
        environ["wsgiorg.futures"] = self.executor.futures
        return self.app(environ, start_response)

# Monolithic build...end of module: rocket/futures.py
# Monolithic build...start of module: rocket/listener.py

# Import System Modules
import os
import socket
import logging
import traceback
from threading import Thread

try:
    import ssl
    from ssl import SSLError
    has_ssl = True
except ImportError:
    has_ssl = False

    class SSLError(socket.error):
        pass
# Import Package Modules
# package imports removed in monolithic build


class Listener(Thread):
    """The Listener class is a class responsible for accepting connections
    and queuing them to be processed by a worker thread."""

    def __init__(self, interface, queue_size, active_queue, *args, **kwargs):
        Thread.__init__(self, *args, **kwargs)

        # Instance variables
        self.active_queue = active_queue
        self.interface = interface
        self.addr = interface[0]
        self.port = interface[1]
        self.secure = len(interface) >= 4
        self.clientcert_req = (len(interface) == 5 and interface[4])

        self.thread = None
        self.ready = False

        # Error Log
        self.err_log = logging.getLogger('Rocket.Errors.Port%i' % self.port)
        self.err_log.addHandler(NullHandler())

        # Build the socket
        if ':' in self.addr:
            listener = socket.socket(socket.AF_INET6, socket.SOCK_STREAM)
        else:
            listener = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        if not listener:
            self.err_log.error("Failed to get socket.")
            return

        if self.secure:
            if not has_ssl:
                self.err_log.error("ssl module required to serve HTTPS.")
                return
            elif not os.path.exists(interface[2]):
                data = (interface[2], interface[0], interface[1])
                self.err_log.error("Cannot find key file "
                                   "'%s'.  Cannot bind to %s:%s" % data)
                return
            elif not os.path.exists(interface[3]):
                data = (interface[3], interface[0], interface[1])
                self.err_log.error("Cannot find certificate file "
                                   "'%s'.  Cannot bind to %s:%s" % data)
                return

            if self.clientcert_req and not os.path.exists(interface[4]):
                data = (interface[4], interface[0], interface[1])
                self.err_log.error("Cannot find root ca certificate file "
                                   "'%s'.  Cannot bind to %s:%s" % data)
                return

        # Set socket options
        try:
            listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        except:
            msg = "Cannot share socket.  Using %s:%i exclusively."
            self.err_log.warning(msg % (self.addr, self.port))

        try:
            if not IS_JYTHON:
                listener.setsockopt(socket.IPPROTO_TCP,
                                    socket.TCP_NODELAY,
                                    1)
        except:
            msg = "Cannot set TCP_NODELAY, things might run a little slower"
            self.err_log.warning(msg)

        try:
            listener.bind((self.addr, self.port))
        except:
            msg = "Socket %s:%i in use by other process and it won't share."
            self.err_log.error(msg % (self.addr, self.port))
        else:
            # We want socket operations to timeout periodically so we can
            # check if the server is shutting down
            listener.settimeout(THREAD_STOP_CHECK_INTERVAL)
            # Listen for new connections allowing queue_size number of
            # connections to wait before rejecting a connection.
            listener.listen(queue_size)

            self.listener = listener

            self.ready = True

    def wrap_socket(self, sock):
        try:
            if self.clientcert_req:
                ca_certs = self.interface[4]
                cert_reqs = ssl.CERT_OPTIONAL
                sock = ssl.wrap_socket(sock,
                                       keyfile=self.interface[2],
                                       certfile=self.interface[3],
                                       server_side=True,
                                       cert_reqs=cert_reqs,
                                       ca_certs=ca_certs,
                                       ssl_version=ssl.PROTOCOL_SSLv23)
            else:
                sock = ssl.wrap_socket(sock,
                                       keyfile=self.interface[2],
                                       certfile=self.interface[3],
                                       server_side=True,
                                       ssl_version=ssl.PROTOCOL_SSLv23)
        except SSLError:
            # Generally this happens when an HTTP request is received on a
            # secure socket. We don't do anything because it will be detected
            # by Worker and dealt with appropriately.
            pass

        return sock

    def start(self):
        if not self.ready:
            self.err_log.warning('Listener started when not ready.')
            return

        if self.thread is not None and self.thread.isAlive():
            self.err_log.warning('Listener already running.')
            return

        self.thread = Thread(target=self.listen, name="Port" + str(self.port))

        self.thread.start()

    def isAlive(self):
        if self.thread is None:
            return False

        return self.thread.isAlive()

    def join(self):
        if self.thread is None:
            return

        self.ready = False

        self.thread.join()

        del self.thread
        self.thread = None
        self.ready = True

    def listen(self):
        if __debug__:
            self.err_log.debug('Entering main loop.')
        while True:
            try:
                sock, addr = self.listener.accept()

                if self.secure:
                    sock = self.wrap_socket(sock)

                self.active_queue.put(((sock, addr),
                                       self.interface[1],
                                       self.secure))

            except socket.timeout:
                # socket.timeout will be raised every
                # THREAD_STOP_CHECK_INTERVAL seconds.  When that happens,
                # we check if it's time to die.

                if not self.ready:
                    if __debug__:
                        self.err_log.debug('Listener exiting.')
                    return
                else:
                    continue
            except:
                self.err_log.error(traceback.format_exc())

# Monolithic build...end of module: rocket/listener.py
# Monolithic build...start of module: rocket/main.py

# Import System Modules
import sys
import time
import socket
import logging
import traceback
from threading import Lock
try:
    from queue import Queue
except ImportError:
    from Queue import Queue

# Import Package Modules
# package imports removed in monolithic build

# Setup Logging
log = logging.getLogger('Rocket')
log.addHandler(NullHandler())


class Rocket(object):
    """The Rocket class is responsible for handling threads and accepting and
    dispatching connections."""

    def __init__(self,
                 interfaces=('127.0.0.1', 8000),
                 method='wsgi',
                 app_info=None,
                 min_threads=None,
                 max_threads=None,
                 queue_size=None,
                 timeout=600,
                 handle_signals=True):

        self.handle_signals = handle_signals
        self.startstop_lock = Lock()
        self.timeout = timeout

        if not isinstance(interfaces, list):
            self.interfaces = [interfaces]
        else:
            self.interfaces = interfaces

        if min_threads is None:
            min_threads = DEFAULTS['MIN_THREADS']

        if max_threads is None:
            max_threads = DEFAULTS['MAX_THREADS']

        if not queue_size:
            if hasattr(socket, 'SOMAXCONN'):
                queue_size = socket.SOMAXCONN
            else:
                queue_size = DEFAULTS['LISTEN_QUEUE_SIZE']

        if max_threads and queue_size > max_threads:
            queue_size = max_threads

        if isinstance(app_info, dict):
            app_info['server_software'] = SERVER_SOFTWARE

        self.monitor_queue = Queue()
        self.active_queue = Queue()

        self._threadpool = ThreadPool(get_method(method),
                                      app_info=app_info,
                                      active_queue=self.active_queue,
                                      monitor_queue=self.monitor_queue,
                                      min_threads=min_threads,
                                      max_threads=max_threads)

        # Build our socket listeners
        self.listeners = [Listener(
            i, queue_size, self.active_queue) for i in self.interfaces]
        for ndx in range(len(self.listeners) - 1, 0, -1):
            if not self.listeners[ndx].ready:
                del self.listeners[ndx]

        if not self.listeners:
            log.critical("No interfaces to listen on...closing.")
            sys.exit(1)

    def _sigterm(self, signum, frame):
        log.info('Received SIGTERM')
        self.stop()

    def _sighup(self, signum, frame):
        log.info('Received SIGHUP')
        self.restart()

    def start(self, background=False):
        log.info('Starting %s' % SERVER_SOFTWARE)

        self.startstop_lock.acquire()

        try:
            # Set up our shutdown signals
            if self.handle_signals:
                try:
                    import signal
                    signal.signal(signal.SIGTERM, self._sigterm)
                    signal.signal(signal.SIGUSR1, self._sighup)
                except:
                    log.debug('This platform does not support signals.')

            # Start our worker threads
            self._threadpool.start()

            # Start our monitor thread
            self._monitor = Monitor(self.monitor_queue,
                                    self.active_queue,
                                    self.timeout,
                                    self._threadpool)
            self._monitor.setDaemon(True)
            self._monitor.start()

            # I know that EXPR and A or B is bad but I'm keeping it for Py2.4
            # compatibility.
            str_extract = lambda l: (l.addr, l.port, l.secure and '*' or '')

            msg = 'Listening on sockets: '
            msg += ', '.join(
                ['%s:%i%s' % str_extract(l) for l in self.listeners])
            log.info(msg)

            for l in self.listeners:
                l.start()

        finally:
            self.startstop_lock.release()

        if background:
            return

        while self._monitor.isAlive():
            try:
                time.sleep(THREAD_STOP_CHECK_INTERVAL)
            except KeyboardInterrupt:
                # Capture a keyboard interrupt when running from a console
                break
            except:
                if self._monitor.isAlive():
                    log.error(traceback.format_exc())
                    continue

        return self.stop()

    def stop(self, stoplogging=False):
        log.info('Stopping %s' % SERVER_SOFTWARE)

        self.startstop_lock.acquire()

        try:
            # Stop listeners
            for l in self.listeners:
                l.ready = False

            # Encourage a context switch
            time.sleep(0.01)

            for l in self.listeners:
                if l.isAlive():
                    l.join()

            # Stop Monitor
            self._monitor.stop()
            if self._monitor.isAlive():
                self._monitor.join()

            # Stop Worker threads
            self._threadpool.stop()

            if stoplogging:
                logging.shutdown()
                msg = "Calling logging.shutdown() is now the responsibility of \
                       the application developer.  Please update your \
                       applications to no longer call rocket.stop(True)"
                try:
                    import warnings
                    raise warnings.DeprecationWarning(msg)
                except ImportError:
                    raise RuntimeError(msg)

        finally:
            self.startstop_lock.release()

    def restart(self):
        self.stop()
        self.start()


def CherryPyWSGIServer(bind_addr,
                       wsgi_app,
                       numthreads=10,
                       server_name=None,
                       max=-1,
                       request_queue_size=5,
                       timeout=10,
                       shutdown_timeout=5):
    """ A Cherrypy wsgiserver-compatible wrapper. """
    max_threads = max
    if max_threads < 0:
        max_threads = 0
    return Rocket(bind_addr, 'wsgi', {'wsgi_app': wsgi_app},
                  min_threads=numthreads,
                  max_threads=max_threads,
                  queue_size=request_queue_size,
                  timeout=timeout)

# Monolithic build...end of module: rocket/main.py
# Monolithic build...start of module: rocket/monitor.py

# Import System Modules
import time
import logging
import select
from threading import Thread

# Import Package Modules
# package imports removed in monolithic build


class Monitor(Thread):
    # Monitor worker class.

    def __init__(self,
                 monitor_queue,
                 active_queue,
                 timeout,
                 threadpool,
                 *args,
                 **kwargs):

        Thread.__init__(self, *args, **kwargs)

        self._threadpool = threadpool

        # Instance Variables
        self.monitor_queue = monitor_queue
        self.active_queue = active_queue
        self.timeout = timeout

        self.log = logging.getLogger('Rocket.Monitor')
        self.log.addHandler(NullHandler())

        self.connections = set()
        self.active = False

    def run(self):
        self.active = True
        conn_list = list()
        list_changed = False

        # We need to make sure the queue is empty before we start
        while not self.monitor_queue.empty():
            self.monitor_queue.get()

        if __debug__:
            self.log.debug('Entering monitor loop.')

        # Enter thread main loop
        while self.active:

            # Move the queued connections to the selection pool
            while not self.monitor_queue.empty():
                if __debug__:
                    self.log.debug('In "receive timed-out connections" loop.')

                c = self.monitor_queue.get()

                if c is None:
                    # A non-client is a signal to die
                    if __debug__:
                        self.log.debug('Received a death threat.')
                    self.stop()
                    break

                self.log.debug('Received a timed out connection.')

                if __debug__:
                    assert(c not in self.connections)

                if IS_JYTHON:
                    # Jython requires a socket to be in Non-blocking mode in
                    # order to select on it.
                    c.setblocking(False)

                if __debug__:
                    self.log.debug('Adding connection to monitor list.')

                self.connections.add(c)
                list_changed = True

            # Wait on those connections
            if list_changed:
                conn_list = list(self.connections)
                list_changed = False

            try:
                if len(conn_list):
                    readable = select.select(conn_list,
                                             [],
                                             [],
                                             THREAD_STOP_CHECK_INTERVAL)[0]
                else:
                    time.sleep(THREAD_STOP_CHECK_INTERVAL)
                    readable = []

                if not self.active:
                    break

                # If we have any readable connections, put them back
                for r in readable:
                    if __debug__:
                        self.log.debug('Restoring readable connection')

                    if IS_JYTHON:
                        # Jython requires a socket to be in Non-blocking mode in
                        # order to select on it, but the rest of the code requires
                        # that it be in blocking mode.
                        r.setblocking(True)

                    r.start_time = time.time()
                    self.active_queue.put(r)

                    self.connections.remove(r)
                    list_changed = True

            except:
                if self.active:
                    raise
                else:
                    break

            # If we have any stale connections, kill them off.
            if self.timeout:
                now = time.time()
                stale = set()
                for c in self.connections:
                    if (now - c.start_time) >= self.timeout:
                        stale.add(c)

                for c in stale:
                    if __debug__:
                        # "EXPR and A or B" kept for Py2.4 compatibility
                        data = (
                            c.client_addr, c.server_port, c.ssl and '*' or '')
                        self.log.debug(
                            'Flushing stale connection: %s:%i%s' % data)

                    self.connections.remove(c)
                    list_changed = True

                    try:
                        c.close()
                    finally:
                        del c

            # Dynamically resize the threadpool to adapt to our changing needs.
            self._threadpool.dynamic_resize()

    def stop(self):
        self.active = False

        if __debug__:
            self.log.debug('Flushing waiting connections')

        while self.connections:
            c = self.connections.pop()
            try:
                c.close()
            finally:
                del c

        if __debug__:
            self.log.debug('Flushing queued connections')

        while not self.monitor_queue.empty():
            c = self.monitor_queue.get()

            if c is None:
                continue

            try:
                c.close()
            finally:
                del c

        # Place a None sentry value to cause the monitor to die.
        self.monitor_queue.put(None)

# Monolithic build...end of module: rocket/monitor.py
# Monolithic build...start of module: rocket/threadpool.py

# Import System Modules
import logging
# Import Package Modules
# package imports removed in monolithic build


# Setup Logging
log = logging.getLogger('Rocket.Errors.ThreadPool')
log.addHandler(NullHandler())


class ThreadPool:
    """The ThreadPool class is a container class for all the worker threads. It
    manages the number of actively running threads."""

    def __init__(self,
                 method,
                 app_info,
                 active_queue,
                 monitor_queue,
                 min_threads=DEFAULTS['MIN_THREADS'],
                 max_threads=DEFAULTS['MAX_THREADS'],
                 ):

        if __debug__:
            log.debug("Initializing ThreadPool.")

        self.check_for_dead_threads = 0
        self.active_queue = active_queue

        self.worker_class = method
        self.min_threads = min_threads
        self.max_threads = max_threads
        self.monitor_queue = monitor_queue
        self.stop_server = False
        self.alive = False

        # TODO - Optimize this based on some real-world usage data
        self.grow_threshold = int(max_threads / 10) + 2

        if not isinstance(app_info, dict):
            app_info = dict()

        if has_futures and app_info.get('futures'):
            app_info['executor'] = WSGIExecutor(max([DEFAULTS['MIN_THREADS'],
                                                     2]))

        app_info.update(max_threads=max_threads,
                        min_threads=min_threads)

        self.min_threads = min_threads
        self.app_info = app_info

        self.threads = set()

    def start(self):
        self.stop_server = False
        if __debug__:
            log.debug("Starting threads.")

        self.grow(self.min_threads)

        self.alive = True

    def stop(self):
        self.alive = False

        if __debug__:
            log.debug("Stopping threads.")

        self.stop_server = True

        # Prompt the threads to die
        self.shrink(len(self.threads))

        # Stop futures initially
        if has_futures and self.app_info.get('futures'):
            if __debug__:
                log.debug("Future executor is present.  Python will not "
                          "exit until all jobs have finished.")
            self.app_info['executor'].shutdown(wait=False)

        # Give them the gun
        # active_threads = [t for t in self.threads if t.isAlive()]
        # while active_threads:
        #     t = active_threads.pop()
        #     t.kill()

        # Wait until they pull the trigger
        for t in self.threads:
            if t.isAlive():
                t.join()

        # Clean up the mess
        self.bring_out_your_dead()

    def bring_out_your_dead(self):
        # Remove dead threads from the pool

        dead_threads = [t for t in self.threads if not t.isAlive()]
        for t in dead_threads:
            if __debug__:
                log.debug("Removing dead thread: %s." % t.getName())
            try:
                # Py2.4 complains here so we put it in a try block
                self.threads.remove(t)
            except:
                pass
        self.check_for_dead_threads -= len(dead_threads)

    def grow(self, amount=None):
        if self.stop_server:
            return

        if not amount:
            amount = self.max_threads

        if self.alive:
            amount = min([amount, self.max_threads - len(self.threads)])

        if __debug__:
            log.debug("Growing by %i." % amount)

        for x in range(amount):
            worker = self.worker_class(self.app_info,
                                       self.active_queue,
                                       self.monitor_queue)

            worker.setDaemon(True)
            self.threads.add(worker)
            worker.start()

    def shrink(self, amount=1):
        if __debug__:
            log.debug("Shrinking by %i." % amount)

        self.check_for_dead_threads += amount

        for x in range(amount):
            self.active_queue.put(None)

    def dynamic_resize(self):
        if (self.max_threads > self.min_threads or self.max_threads == 0):
            if self.check_for_dead_threads > 0:
                self.bring_out_your_dead()

            queueSize = self.active_queue.qsize()
            threadCount = len(self.threads)

            if __debug__:
                log.debug("Examining ThreadPool. %i threads and %i Q'd conxions"
                          % (threadCount, queueSize))

            if queueSize == 0 and threadCount > self.min_threads:
                self.shrink()

            elif queueSize > self.grow_threshold:

                self.grow(queueSize)

# Monolithic build...end of module: rocket/threadpool.py
# Monolithic build...start of module: rocket/worker.py

# Import System Modules
import re
import sys
import socket
import logging
import traceback
from wsgiref.headers import Headers
from threading import Thread
from datetime import datetime

try:
    from urllib import unquote
except ImportError:
    from urllib.parse import unquote

try:
    from io import StringIO
except ImportError:
    try:
        from cStringIO import StringIO
    except ImportError:
        from StringIO import StringIO

try:
    from ssl import SSLError
except ImportError:
    class SSLError(socket.error):
        pass
# Import Package Modules
# package imports removed in monolithic build


# Define Constants
re_SLASH = re.compile('%2F', re.IGNORECASE)
re_REQUEST_LINE = re.compile(r"""^
(?P<method>OPTIONS|GET|HEAD|POST|PUT|DELETE|TRACE|CONNECT)   # Request Method
\                                                            # (single space)
(
    (?P<scheme>[^:/]+)                                       # Scheme
    (://)  #
    (?P<host>[^/]+)                                          # Host
)? #
(?P<path>(\*|/[^ \?]*))                                      # Path
(\? (?P<query_string>[^ ]*))?                                # Query String
\                                                            # (single space)
(?P<protocol>HTTPS?/1\.[01])                                 # Protocol
$
""", re.X)
LOG_LINE = '%(client_ip)s - "%(request_line)s" - %(status)s %(size)s'
RESPONSE = '''\
%s %s
Content-Length: %i
Content-Type: %s

%s
'''
if IS_JYTHON:
    HTTP_METHODS = set(['OPTIONS', 'GET', 'HEAD', 'POST', 'PUT',
                        'DELETE', 'TRACE', 'CONNECT'])


class Worker(Thread):
    """The Worker class is a base class responsible for receiving connections
    and (a subclass) will run an application to process the the connection """

    def __init__(self,
                 app_info,
                 active_queue,
                 monitor_queue,
                 *args,
                 **kwargs):

        Thread.__init__(self, *args, **kwargs)

        # Instance Variables
        self.app_info = app_info
        self.active_queue = active_queue
        self.monitor_queue = monitor_queue

        self.size = 0
        self.status = "200 OK"
        self.closeConnection = True
        self.request_line = ""
        self.protocol = 'HTTP/1.1'

        # Request Log
        self.req_log = logging.getLogger('Rocket.Requests')
        self.req_log.addHandler(NullHandler())

        # Error Log
        self.err_log = logging.getLogger('Rocket.Errors.' + self.getName())
        self.err_log.addHandler(NullHandler())

    def _handleError(self, typ, val, tb):
        if typ == SSLError:
            if 'timed out' in str(val.args[0]):
                typ = SocketTimeout
        if typ == SocketTimeout:
            if __debug__:
                self.err_log.debug('Socket timed out')
            self.monitor_queue.put(self.conn)
            return True
        if typ == SocketClosed:
            self.closeConnection = True
            if __debug__:
                self.err_log.debug('Client closed socket')
            return False
        if typ == BadRequest:
            self.closeConnection = True
            if __debug__:
                self.err_log.debug('Client sent a bad request')
            return True
        if typ == socket.error:
            self.closeConnection = True
            if val.args[0] in IGNORE_ERRORS_ON_CLOSE:
                if __debug__:
                    self.err_log.debug('Ignorable socket Error received...'
                                       'closing connection.')
                return False
            else:
                self.status = "999 Utter Server Failure"
                tb_fmt = traceback.format_exception(typ, val, tb)
                self.err_log.error('Unhandled Error when serving '
                                   'connection:\n' + '\n'.join(tb_fmt))
                return False

        self.closeConnection = True
        tb_fmt = traceback.format_exception(typ, val, tb)
        self.err_log.error('\n'.join(tb_fmt))
        self.send_response('500 Server Error')
        return False

    def run(self):
        if __debug__:
            self.err_log.debug('Entering main loop.')

        # Enter thread main loop
        while True:
            conn = self.active_queue.get()

            if not conn:
                # A non-client is a signal to die
                if __debug__:
                    self.err_log.debug('Received a death threat.')
                return conn

            if isinstance(conn, tuple):
                conn = Connection(*conn)

            self.conn = conn

            if conn.ssl != conn.secure:
                self.err_log.info('Received HTTP connection on HTTPS port.')
                self.send_response('400 Bad Request')
                self.closeConnection = True
                conn.close()
                continue
            else:
                if __debug__:
                    self.err_log.debug('Received a connection.')
                self.closeConnection = False

            # Enter connection serve loop
            while True:
                if __debug__:
                    self.err_log.debug('Serving a request')
                try:
                    self.run_app(conn)
                except:
                    exc = sys.exc_info()
                    handled = self._handleError(*exc)
                    if handled:
                        break
                finally:
                    if self.request_line:
                        log_info = dict(client_ip=conn.client_addr,
                                        time=datetime.now().strftime('%c'),
                                        status=self.status.split(' ')[0],
                                        size=self.size,
                                        request_line=self.request_line)
                        self.req_log.info(LOG_LINE % log_info)

                if self.closeConnection:
                    try:
                        conn.close()
                    except:
                        self.err_log.error(str(traceback.format_exc()))

                    break

    def run_app(self, conn):
        # Must be overridden with a method reads the request from the socket
        # and sends a response.
        self.closeConnection = True
        raise NotImplementedError('Overload this method!')

    def send_response(self, status):
        stat_msg = status.split(' ', 1)[1]
        msg = RESPONSE % (self.protocol,
                          status,
                          len(stat_msg),
                          'text/plain',
                          stat_msg)
        try:
            self.conn.sendall(b(msg))
        except socket.timeout:
            self.closeConnection = True
            msg = 'Tried to send "%s" to client but received timeout error'
            self.err_log.error(msg % status)
        except socket.error:
            self.closeConnection = True
            msg = 'Tried to send "%s" to client but received socket error'
            self.err_log.error(msg % status)

    def read_request_line(self, sock_file):
        self.request_line = ''
        try:
            # Grab the request line
            d = sock_file.readline()
            if PY3K:
                d = d.decode('ISO-8859-1')

            if d == '\r\n':
                # Allow an extra NEWLINE at the beginning per HTTP 1.1 spec
                if __debug__:
                    self.err_log.debug('Client sent newline')

                d = sock_file.readline()
                if PY3K:
                    d = d.decode('ISO-8859-1')
        except socket.timeout:
            raise SocketTimeout('Socket timed out before request.')
        except TypeError:
            raise SocketClosed(
                'SSL bug caused closure of socket.  See '
                '"https://groups.google.com/d/topic/web2py/P_Gw0JxWzCs".')

        d = d.strip()

        if not d:
            if __debug__:
                self.err_log.debug(
                    'Client did not send a recognizable request.')
            raise SocketClosed('Client closed socket.')

        self.request_line = d

        # NOTE: I've replaced the traditional method of procedurally breaking
        # apart the request line with a (rather unsightly) regular expression.
        # However, Java's regexp support sucks so bad that it actually takes
        # longer in Jython to process the regexp than procedurally. So I've
        # left the old code here for Jython's sake...for now.
        if IS_JYTHON:
            return self._read_request_line_jython(d)

        match = re_REQUEST_LINE.match(d)

        if not match:
            self.send_response('400 Bad Request')
            raise BadRequest

        req = match.groupdict()
        for k, v in req.iteritems():
            if not v:
                req[k] = ""
            if k == 'path':
                req['path'] = r'%2F'.join(
                    [unquote(x) for x in re_SLASH.split(v)])

        self.protocol = req['protocol']
        return req

    def _read_request_line_jython(self, d):
        d = d.strip()
        try:
            method, uri, proto = d.split(' ')
            if not proto.startswith('HTTP') or \
                    proto[-3:] not in ('1.0', '1.1') or \
                    method not in HTTP_METHODS:
                self.send_response('400 Bad Request')
                raise BadRequest
        except ValueError:
            self.send_response('400 Bad Request')
            raise BadRequest

        req = dict(method=method, protocol=proto)
        scheme = ''
        host = ''
        if uri == '*' or uri.startswith('/'):
            path = uri
        elif '://' in uri:
            scheme, rest = uri.split('://')
            host, path = rest.split('/', 1)
            path = '/' + path
        else:
            self.send_response('400 Bad Request')
            raise BadRequest

        query_string = ''
        if '?' in path:
            path, query_string = path.split('?', 1)

        path = r'%2F'.join([unquote(x) for x in re_SLASH.split(path)])

        req.update(path=path,
                   query_string=query_string,
                   scheme=scheme.lower(),
                   host=host)
        return req

    def read_headers(self, sock_file):
        try:
            headers = dict()
            lname = None
            lval = None
            while True:
                l = sock_file.readline()

                if PY3K:
                    try:
                        l = str(l, 'ISO-8859-1')
                    except UnicodeDecodeError:
                        self.err_log.warning(
                            'Client sent invalid header: ' + repr(l))

                if l.strip().replace('\0', '') == '':
                    break

                if l[0] in ' \t' and lname:
                    # Some headers take more than one line
                    lval += ' ' + l.strip()
                else:
                    # HTTP header values are latin-1 encoded
                    l = l.split(':', 1)
                    # HTTP header names are us-ascii encoded

                    lname = l[0].strip().upper().replace('-', '_')
                    lval = l[-1].strip()

                headers[str(lname)] = str(lval)

        except socket.timeout:
            raise SocketTimeout("Socket timed out before request.")

        return headers


class SocketTimeout(Exception):
    """Exception for when a socket times out between requests."""
    pass


class BadRequest(Exception):
    """Exception for when a client sends an incomprehensible request."""
    pass


class SocketClosed(Exception):
    """Exception for when a socket is closed by the client."""
    pass


class ChunkedReader(object):

    def __init__(self, sock_file):
        self.stream = sock_file
        self.chunk_size = 0

    def _read_header(self):
        chunk_len = ""
        try:
            while "" == chunk_len:
                chunk_len = self.stream.readline().strip()
            return int(chunk_len, 16)
        except ValueError:
            return 0

    def read(self, size):
        data = b('')
        chunk_size = self.chunk_size
        while size:
            if not chunk_size:
                chunk_size = self._read_header()

            if size < chunk_size:
                data += self.stream.read(size)
                chunk_size -= size
                break
            else:
                if not chunk_size:
                    break
                data += self.stream.read(chunk_size)
                size -= chunk_size
                chunk_size = 0

        self.chunk_size = chunk_size
        return data

    def readline(self):
        data = b('')
        c = self.read(1)
        while c and c != b('\n'):
            data += c
            c = self.read(1)
        data += c
        return data

    def readlines(self):
        yield self.readline()


def get_method(method):
    methods = dict(wsgi=WSGIWorker)
    return methods[method.lower()]

# Monolithic build...end of module: rocket/worker.py
# Monolithic build...start of module: rocket/methods/__init__.py

# Monolithic build...end of module: rocket/methods/__init__.py
# Monolithic build...start of module: rocket/methods/wsgi.py

# Import System Modules
import sys
import socket
from wsgiref.headers import Headers
from wsgiref.util import FileWrapper

# Import Package Modules
# package imports removed in monolithic build

if PY3K:
    from email.utils import formatdate
else:
    # Caps Utils for Py2.4 compatibility
    from email.Utils import formatdate

# Define Constants
NEWLINE = b('\r\n')
HEADER_RESPONSE = '''HTTP/1.1 %s\r\n%s'''
BASE_ENV = {'SERVER_NAME': SERVER_NAME,
            'SCRIPT_NAME': '',  # Direct call WSGI does not need a name
            'wsgi.errors': sys.stderr,
            'wsgi.version': (1, 0),
            'wsgi.multiprocess': False,
            'wsgi.run_once': False,
            'wsgi.file_wrapper': FileWrapper
            }


class WSGIWorker(Worker):
    def __init__(self, *args, **kwargs):
        """Builds some instance variables that will last the life of the
        thread."""
        Worker.__init__(self, *args, **kwargs)

        if isinstance(self.app_info, dict):
            multithreaded = self.app_info.get('max_threads') != 1
        else:
            multithreaded = False
        self.base_environ = dict(
            {'SERVER_SOFTWARE': self.app_info['server_software'],
             'wsgi.multithread': multithreaded,
             })
        self.base_environ.update(BASE_ENV)

        # Grab our application
        self.app = self.app_info.get('wsgi_app')

        if not hasattr(self.app, "__call__"):
            raise TypeError("The wsgi_app specified (%s) is not a valid WSGI application." % repr(self.app))

        # Enable futures
        if has_futures and self.app_info.get('futures'):
            executor = self.app_info['executor']
            self.base_environ.update({"wsgiorg.executor": executor,
                                      "wsgiorg.futures": executor.futures})

    def build_environ(self, sock_file, conn):
        """ Build the execution environment. """
        # Grab the request line
        request = self.read_request_line(sock_file)

        # Copy the Base Environment
        environ = self.base_environ.copy()

        # Grab the headers
        for k, v in self.read_headers(sock_file).iteritems():
            environ[str('HTTP_' + k)] = v

        # Add CGI Variables
        environ['REQUEST_METHOD'] = request['method']
        environ['PATH_INFO'] = request['path']
        environ['SERVER_PROTOCOL'] = request['protocol']
        environ['SERVER_PORT'] = str(conn.server_port)
        environ['REMOTE_PORT'] = str(conn.client_port)
        environ['REMOTE_ADDR'] = str(conn.client_addr)
        environ['QUERY_STRING'] = request['query_string']
        if 'HTTP_CONTENT_LENGTH' in environ:
            environ['CONTENT_LENGTH'] = environ['HTTP_CONTENT_LENGTH']
        if 'HTTP_CONTENT_TYPE' in environ:
            environ['CONTENT_TYPE'] = environ['HTTP_CONTENT_TYPE']

        # Save the request method for later
        self.request_method = environ['REQUEST_METHOD']

        # Add Dynamic WSGI Variables
        if conn.ssl:
            environ['wsgi.url_scheme'] = 'https'
            environ['HTTPS'] = 'on'
            try:
                peercert = conn.socket.getpeercert(binary_form=True)
                environ['SSL_CLIENT_RAW_CERT'] = \
                    peercert and ssl.DER_cert_to_PEM_cert(peercert)
            except Exception:
                print sys.exc_info()[1]
        else:
            environ['wsgi.url_scheme'] = 'http'

        if environ.get('HTTP_TRANSFER_ENCODING', '') == 'chunked':
            environ['wsgi.input'] = ChunkedReader(sock_file)
        else:
            environ['wsgi.input'] = sock_file

        return environ

    def send_headers(self, data, sections):
        h_set = self.header_set

        # Does the app want us to send output chunked?
        self.chunked = h_set.get('Transfer-Encoding', '').lower() == 'chunked'

        # Add a Date header if it's not there already
        if not 'Date' in h_set:
            h_set['Date'] = formatdate(usegmt=True)

        # Add a Server header if it's not there already
        if not 'Server' in h_set:
            h_set['Server'] = HTTP_SERVER_SOFTWARE

        if 'Content-Length' in h_set:
            self.size = int(h_set['Content-Length'])
        else:
            s = int(self.status.split(' ')[0])
            if (s < 200 or s not in (204, 205, 304)) and not self.chunked:
                if sections == 1 or self.protocol != 'HTTP/1.1':
                    # Add a Content-Length header because it's not there
                    self.size = len(data)
                    h_set['Content-Length'] = str(self.size)
                else:
                    # If they sent us more than one section, we blow chunks
                    h_set['Transfer-Encoding'] = 'Chunked'
                    self.chunked = True
                    if __debug__:
                        self.err_log.debug('Adding header...'
                                           'Transfer-Encoding: Chunked')

        if 'Connection' not in h_set:
            # If the application did not provide a connection header,
            # fill it in
            client_conn = self.environ.get('HTTP_CONNECTION', '').lower()
            if self.environ['SERVER_PROTOCOL'] == 'HTTP/1.1':
                # HTTP = 1.1 defaults to keep-alive connections
                if client_conn:
                    h_set['Connection'] = client_conn
                else:
                    h_set['Connection'] = 'keep-alive'
            else:
                # HTTP < 1.1 supports keep-alive but it's quirky
                # so we don't support it
                h_set['Connection'] = 'close'

        # Close our connection if we need to.
        self.closeConnection = h_set.get('Connection', '').lower() == 'close'

        # Build our output headers
        header_data = HEADER_RESPONSE % (self.status, str(h_set))

        # Send the headers
        if __debug__:
            self.err_log.debug('Sending Headers: %s' % repr(header_data))
        self.conn.sendall(b(header_data))
        self.headers_sent = True

    def write_warning(self, data, sections=None):
        self.err_log.warning('WSGI app called write method directly.  This is '
                             'deprecated behavior.  Please update your app.')
        return self.write(data, sections)

    def write(self, data, sections=None):
        """ Write the data to the output socket. """

        if self.error[0]:
            self.status = self.error[0]
            data = b(self.error[1])

        if not self.headers_sent:
            self.send_headers(data, sections)

        if self.request_method != 'HEAD':
            try:
                if self.chunked:
                    self.conn.sendall(b('%x\r\n%s\r\n' % (len(data), data)))
                else:
                    self.conn.sendall(data)
            except socket.timeout:
                self.closeConnection = True
            except socket.error:
                # But some clients will close the connection before that
                # resulting in a socket error.
                self.closeConnection = True

    def start_response(self, status, response_headers, exc_info=None):
        """ Store the HTTP status and headers to be sent when self.write is
        called. """
        if exc_info:
            try:
                if self.headers_sent:
                    # Re-raise original exception if headers sent
                    # because this violates WSGI specification.
                    raise
            finally:
                exc_info = None
        elif self.header_set:
            raise AssertionError("Headers already set!")

        if PY3K and not isinstance(status, str):
            self.status = str(status, 'ISO-8859-1')
        else:
            self.status = status
        # Make sure headers are bytes objects
        try:
            self.header_set = Headers(response_headers)
        except UnicodeDecodeError:
            self.error = ('500 Internal Server Error',
                          'HTTP Headers should be bytes')
            self.err_log.error('Received HTTP Headers from client that contain'
                               ' invalid characters for Latin-1 encoding.')

        return self.write_warning

    def run_app(self, conn):
        self.size = 0
        self.header_set = Headers([])
        self.headers_sent = False
        self.error = (None, None)
        self.chunked = False
        sections = None
        output = None

        if __debug__:
            self.err_log.debug('Getting sock_file')

        # Build our file-like object
        if PY3K:
            sock_file = conn.makefile(mode='rb', buffering=BUF_SIZE)
        else:
            sock_file = conn.makefile(BUF_SIZE)

        try:
            # Read the headers and build our WSGI environment
            self.environ = environ = self.build_environ(sock_file, conn)

            # Handle 100 Continue
            if environ.get('HTTP_EXPECT', '') == '100-continue':
                res = environ['SERVER_PROTOCOL'] + ' 100 Continue\r\n\r\n'
                conn.sendall(b(res))

            # Send it to our WSGI application
            output = self.app(environ, self.start_response)

            if not hasattr(output, '__len__') and not hasattr(output, '__iter__'):
                self.error = ('500 Internal Server Error',
                              'WSGI applications must return a list or '
                              'generator type.')

            if hasattr(output, '__len__'):
                sections = len(output)

            for data in output:
                # Don't send headers until body appears
                if data:
                    self.write(data, sections)

            if not self.headers_sent:
                # Send headers if the body was empty
                self.send_headers('', sections)

            if self.chunked and self.request_method != 'HEAD':
                # If chunked, send our final chunk length
                self.conn.sendall(b('0\r\n\r\n'))

        # Don't capture exceptions here.  The Worker class handles
        # them appropriately.
        finally:
            if __debug__:
                self.err_log.debug('Finally closing output and sock_file')

            if hasattr(output, 'close'):
                output.close()

            sock_file.close()

# Monolithic build...end of module: rocket/methods/wsgi.py
