from gluon.admin import *
from gluon.fileutils import abspath, read_file, write_file
from gluon.tools import Service
from glob import glob
import shutil
import platform
import time
import base64
import os
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO


service = Service(globals())


@service.jsonrpc
def login():
    "dummy function to test credentials"
    return True


@service.jsonrpc
def list_apps():
    "list installed applications"
    regex = re.compile('^\w+$')
    apps = [f for f in os.listdir(apath(r=request)) if regex.match(f)]
    return apps


@service.jsonrpc
def list_files(app, pattern='.*\.py$'):
    files = listdir(apath('%s/' % app, r=request), pattern)
    return [x.replace('\\', '/') for x in files]


@service.jsonrpc
def read_file(filename, b64=False):
    """ Visualize object code """
    f = open(apath(filename, r=request), "rb")
    try:
        data = f.read()
        if not b64:
            data = data.replace('\r', '')
        else:
            data = base64.b64encode(data)
    finally:
        f.close()
    return data


@service.jsonrpc
def write_file(filename, data, b64=False):
    f = open(apath(filename, r=request), "wb")
    try:
        if not b64:
            data = data.replace('\r\n', '\n').strip() + '\n'
        else:
            data = base64.b64decode(data)
        f.write(data)
    finally:
        f.close()


@service.jsonrpc
def hash_file(filename):
    data = read_file(filename)
    file_hash = md5_hash(data)
    path = apath(filename, r=request)
    saved_on = os.stat(path)[stat.ST_MTIME]
    size = os.path.getsize(path)
    return dict(saved_on=saved_on, file_hash=file_hash, size=size)


@service.jsonrpc
def install(app_name, filename, data, overwrite=True):
    f = StringIO(base64.b64decode(data))
    installed = app_install(app_name, f, request, filename,
                            overwrite=overwrite)

    return installed


@service.jsonrpc
def attach_debugger(host='localhost', port=6000, authkey='secret password'):
    import gluon.contrib.qdb as qdb
    import gluon.debug
    from multiprocessing.connection import Listener

    if isinstance(authkey, unicode):
        authkey = authkey.encode('utf8')

    if not hasattr(gluon.debug, 'qdb_listener'):
        # create a remote debugger server and wait for connection
        address = (host, port)     # family is deduced to be 'AF_INET'
        gluon.debug.qdb_listener = Listener(address, authkey=authkey)
        gluon.debug.qdb_connection = gluon.debug.qdb_listener.accept()
        # create the backend
        gluon.debug.qdb_debugger = qdb.Qdb(gluon.debug.qdb_connection)
        gluon.debug.dbg = gluon.debug.qdb_debugger
        # welcome message (this should be displayed on the frontend)
        print 'debugger connected to', gluon.debug.qdb_listener.last_accepted
    return True     # connection successful!


@service.jsonrpc
def detach_debugger():
    import gluon.contrib.qdb as qdb
    import gluon.debug
    # stop current debugger
    if gluon.debug.qdb_debugger:
        try:
            gluon.debug.qdb_debugger.do_quit()
        except:
            pass
    if hasattr(gluon.debug, 'qdb_listener'):
        if gluon.debug.qdb_connection:
            gluon.debug.qdb_connection.close()
            del gluon.debug.qdb_connection
        if gluon.debug.qdb_listener:
            gluon.debug.qdb_listener.close()
            del gluon.debug.qdb_listener
    gluon.debug.qdb_debugger = None
    return True


def call():
    session.forget()
    return service()
