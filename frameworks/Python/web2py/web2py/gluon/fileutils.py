#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

File operations
---------------
"""

import storage
import os
import re
import tarfile
import glob
import time
import datetime
import logging
from http import HTTP
from gzip import open as gzopen
from recfile import generate

__all__ = [
    'parse_version',
    'read_file',
    'write_file',
    'readlines_file',
    'up',
    'abspath',
    'mktree',
    'listdir',
    'recursive_unlink',
    'cleanpath',
    'tar',
    'untar',
    'tar_compiled',
    'get_session',
    'check_credentials',
    'w2p_pack',
    'w2p_unpack',
    'w2p_pack_plugin',
    'w2p_unpack_plugin',
    'fix_newlines',
    'make_fake_file_like_object',
]


def parse_semantic(version="Version 1.99.0-rc.1+timestamp.2011.09.19.08.23.26"):
    """Parses a version string according to http://semver.org/ rules

    Args:
        version(str): the SemVer string

    Returns:
        tuple: Major, Minor, Patch, Release, Build Date

    """
    re_version = re.compile('(\d+)\.(\d+)\.(\d+)(\-(?P<pre>[^\s+]*))?(\+(?P<build>\S*))')
    m = re_version.match(version.strip().split()[-1])
    if not m:
        return None
    a, b, c = int(m.group(1)), int(m.group(2)), int(m.group(3))
    pre_release = m.group('pre') or ''
    build = m.group('build') or ''
    if build.startswith('timestamp'):
        build = datetime.datetime.strptime(build.split('.', 1)[1], '%Y.%m.%d.%H.%M.%S')
    return (a, b, c, pre_release, build)


def parse_legacy(version="Version 1.99.0 (2011-09-19 08:23:26)"):
    """Parses "legacy" version string

    Args:
        version(str): the version string

    Returns:
        tuple: Major, Minor, Patch, Release, Build Date

    """
    re_version = re.compile('[^\d]+ (\d+)\.(\d+)\.(\d+)\s*\((?P<datetime>.+?)\)\s*(?P<type>[a-z]+)?')
    m = re_version.match(version)
    a, b, c = int(m.group(1)), int(m.group(2)), int(m.group(3)),
    pre_release = m.group('type') or 'dev'
    build = datetime.datetime.strptime(m.group('datetime'), '%Y-%m-%d %H:%M:%S')
    return (a, b, c, pre_release, build)


def parse_version(version):
    """Attempts to parse SemVer, fallbacks on legacy
    """
    version_tuple = parse_semantic(version)
    if not version_tuple:
        version_tuple = parse_legacy(version)
    return version_tuple


def read_file(filename, mode='r'):
    """Returns content from filename, making sure to close the file explicitly
    on exit.
    """
    f = open(filename, mode)
    try:
        return f.read()
    finally:
        f.close()


def write_file(filename, value, mode='w'):
    """Writes <value> to filename, making sure to close the file
    explicitly on exit.
    """
    f = open(filename, mode)
    try:
        return f.write(value)
    finally:
        f.close()


def readlines_file(filename, mode='r'):
    """Applies .split('\n') to the output of `read_file()`
    """
    return read_file(filename, mode).split('\n')


def mktree(path):
    head, tail = os.path.split(path)
    if head:
        if tail:
            mktree(head)
        if not os.path.exists(head):
            os.mkdir(head)


def listdir(path,
            expression='^.+$',
            drop=True,
            add_dirs=False,
            sort=True,
            maxnum=None,
            exclude_content_from=None
            ):
    """
    Like `os.listdir()` but you can specify a regex pattern to filter files.
    If `add_dirs` is True, the returned items will have the full path.
    """
    if exclude_content_from is None:
        exclude_content_from = []
    if path[-1:] != os.path.sep:
        path = path + os.path.sep
    if drop:
        n = len(path)
    else:
        n = 0
    regex = re.compile(expression)
    items = []
    for (root, dirs, files) in os.walk(path, topdown=True):
        for dir in dirs[:]:
            if dir.startswith('.'):
                dirs.remove(dir)
        if add_dirs:
            items.append(root[n:])
        for file in sorted(files):
            if regex.match(file) and not file.startswith('.'):
                if root not in exclude_content_from:
                    items.append(os.path.join(root, file)[n:])
            if maxnum and len(items) >= maxnum:
                break
    if sort:
        return sorted(items)
    else:
        return items


def recursive_unlink(f):
    """Deletes `f`. If it's a folder, also its contents will be deleted
    """
    if os.path.isdir(f):
        for s in os.listdir(f):
            recursive_unlink(os.path.join(f, s))
        os.rmdir(f)
    elif os.path.isfile(f):
        os.unlink(f)


def cleanpath(path):
    """Turns any expression/path into a valid filename. replaces / with _ and
    removes special characters.
    """

    items = path.split('.')
    if len(items) > 1:
        path = re.sub('[^\w\.]+', '_', '_'.join(items[:-1]) + '.'
                      + ''.join(items[-1:]))
    else:
        path = re.sub('[^\w\.]+', '_', ''.join(items[-1:]))
    return path


def _extractall(filename, path='.', members=None):
    tar = tarfile.TarFile(filename, 'r')
    ret = tar.extractall(path, members)
    tar.close()
    return ret


def tar(file, dir, expression='^.+$',
        filenames=None, exclude_content_from=None):
    """Tars dir into file, only tars file that match expression
    """

    tar = tarfile.TarFile(file, 'w')
    try:
        if filenames is None:
            filenames = listdir(dir, expression, add_dirs=True,
                exclude_content_from=exclude_content_from)
        for file in filenames:
            tar.add(os.path.join(dir, file), file, False)
    finally:
        tar.close()


def untar(file, dir):
    """Untar file into dir
    """

    _extractall(file, dir)


def w2p_pack(filename, path, compiled=False, filenames=None):
    """Packs a web2py application.

    Args:
        filename(str): path to the resulting archive
        path(str): path to the application
        compiled(bool): if `True` packs the compiled version
        filenames(list): adds filenames to the archive
    """
    filename = abspath(filename)
    path = abspath(path)
    tarname = filename + '.tar'
    if compiled:
        tar_compiled(tarname, path, '^[\w\.\-]+$',
                     exclude_content_from=['cache', 'sessions', 'errors'])
    else:
        tar(tarname, path, '^[\w\.\-]+$', filenames=filenames,
            exclude_content_from=['cache', 'sessions', 'errors'])
    w2pfp = gzopen(filename, 'wb')
    tarfp = open(tarname, 'rb')
    w2pfp.write(tarfp.read())
    w2pfp.close()
    tarfp.close()
    os.unlink(tarname)


def create_welcome_w2p():
    if not os.path.exists('welcome.w2p') or os.path.exists('NEWINSTALL'):
        try:
            w2p_pack('welcome.w2p', 'applications/welcome')
            os.unlink('NEWINSTALL')
            logging.info("New installation: created welcome.w2p file")
        except:
            logging.error("New installation error: unable to create welcome.w2p file")


def w2p_unpack(filename, path, delete_tar=True):

    if filename == 'welcome.w2p':
        create_welcome_w2p()
    filename = abspath(filename)
    path = abspath(path)
    if filename[-4:] == '.w2p' or filename[-3:] == '.gz':
        if filename[-4:] == '.w2p':
            tarname = filename[:-4] + '.tar'
        else:
            tarname = filename[:-3] + '.tar'
        fgzipped = gzopen(filename, 'rb')
        tarfile = open(tarname, 'wb')
        tarfile.write(fgzipped.read())
        tarfile.close()
        fgzipped.close()
    else:
        tarname = filename
    untar(tarname, path)
    if delete_tar:
        os.unlink(tarname)


def w2p_pack_plugin(filename, path, plugin_name):
    """Packs the given plugin into a w2p file.
    Will match files at::

        <path>/*/plugin_[name].*
        <path>/*/plugin_[name]/*

    """
    filename = abspath(filename)
    path = abspath(path)
    if not filename.endswith('web2py.plugin.%s.w2p' % plugin_name):
        raise Exception("Not a web2py plugin name")
    plugin_tarball = tarfile.open(filename, 'w:gz')
    try:
        app_dir = path
        while app_dir[-1] == '/':
            app_dir = app_dir[:-1]
        files1 = glob.glob(
            os.path.join(app_dir, '*/plugin_%s.*' % plugin_name))
        files2 = glob.glob(
            os.path.join(app_dir, '*/plugin_%s/*' % plugin_name))
        for file in files1 + files2:
            plugin_tarball.add(file, arcname=file[len(app_dir) + 1:])
    finally:
        plugin_tarball.close()


def w2p_unpack_plugin(filename, path, delete_tar=True):
    filename = abspath(filename)
    path = abspath(path)
    if not os.path.basename(filename).startswith('web2py.plugin.'):
        raise Exception("Not a web2py plugin")
    w2p_unpack(filename, path, delete_tar)


def tar_compiled(file, dir, expression='^.+$',
                 exclude_content_from=None):
    """Used to tar a compiled application.
    The content of models, views, controllers is not stored in the tar file.
    """

    tar = tarfile.TarFile(file, 'w')
    for file in listdir(dir, expression, add_dirs=True,
                        exclude_content_from=exclude_content_from):
        filename = os.path.join(dir, file)
        if os.path.islink(filename):
            continue
        if os.path.isfile(filename) and file[-4:] != '.pyc':
            if file[:6] == 'models':
                continue
            if file[:5] == 'views':
                continue
            if file[:11] == 'controllers':
                continue
            if file[:7] == 'modules':
                continue
        tar.add(filename, file, False)
    tar.close()


def up(path):
    return os.path.dirname(os.path.normpath(path))


def get_session(request, other_application='admin'):
    """Checks that user is authorized to access other_application"""
    if request.application == other_application:
        raise KeyError
    try:
        session_id = request.cookies['session_id_' + other_application].value
        session_filename = os.path.join(
            up(request.folder), other_application, 'sessions', session_id)
        if not os.path.exists(session_filename):
            session_filename = generate(session_filename)
        osession = storage.load_storage(session_filename)
    except Exception, e:
        osession = storage.Storage()
    return osession


def set_session(request, session, other_application='admin'):
    """Checks that user is authorized to access other_application"""
    if request.application == other_application:
        raise KeyError
    session_id = request.cookies['session_id_' + other_application].value
    session_filename = os.path.join(
        up(request.folder), other_application, 'sessions', session_id)
    storage.save_storage(session, session_filename)


def check_credentials(request, other_application='admin',
                      expiration=60 * 60, gae_login=True):
    """Checks that user is authorized to access other_application"""
    if request.env.web2py_runtime_gae:
        from google.appengine.api import users
        if users.is_current_user_admin():
            return True
        elif gae_login:
            login_html = '<a href="%s">Sign in with your google account</a>.' \
                % users.create_login_url(request.env.path_info)
            raise HTTP(200, '<html><body>%s</body></html>' % login_html)
        else:
            return False
    else:
        t0 = time.time()
        dt = t0 - expiration
        s = get_session(request, other_application)
        r = (s.authorized and s.last_time and s.last_time > dt)
        if r:
            s.last_time = t0
            set_session(request, s, other_application)
        return r


def fix_newlines(path):
    regex = re.compile(r'''(\r
|\r|
)''')
    for filename in listdir(path, '.*\.(py|html)$', drop=False):
        rdata = read_file(filename, 'rb')
        wdata = regex.sub('\n', rdata)
        if wdata != rdata:
            write_file(filename, wdata, 'wb')


def copystream(
    src,
    dest,
    size,
    chunk_size=10 ** 5,
):
    """
    this is here because I think there is a bug in shutil.copyfileobj
    """
    while size > 0:
        if size < chunk_size:
            data = src.read(size)
        else:
            data = src.read(chunk_size)
        length = len(data)
        if length > size:
            (data, length) = (data[:size], size)
        size -= length
        if length == 0:
            break
        dest.write(data)
        if length < chunk_size:
            break
    dest.seek(0)
    return


def make_fake_file_like_object():
    class LogFile(object):
        def write(self, value):
            pass

        def close(self):
            pass
    return LogFile()


from settings import global_settings  # we need to import settings here because
                                      # settings imports fileutils too


def abspath(*relpath, **base):
    """Converts relative path to absolute path based (by default) on
    applications_parent
    """
    path = os.path.join(*relpath)
    gluon = base.get('gluon', False)
    if os.path.isabs(path):
        return path
    if gluon:
        return os.path.join(global_settings.gluon_parent, path)
    return os.path.join(global_settings.applications_parent, path)
