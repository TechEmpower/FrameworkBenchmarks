#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Utility functions for the Admin application
-------------------------------------------
"""
import os
import sys
import traceback
import zipfile
import urllib
from shutil import rmtree
from gluon.utils import web2py_uuid
from gluon.fileutils import w2p_pack, w2p_unpack, w2p_pack_plugin, w2p_unpack_plugin
from gluon.fileutils import up, fix_newlines, abspath, recursive_unlink
from gluon.fileutils import read_file, write_file, parse_version
from gluon.restricted import RestrictedError
from gluon.settings import global_settings
from gluon.cache import CacheOnDisk


if not global_settings.web2py_runtime_gae:
    import site


def apath(path='', r=None):
    """Builds a path inside an application folder

    Args:
        path(str): path within the application folder
        r: the global request object

    """

    opath = up(r.folder)
    while path[:3] == '../':
        (opath, path) = (up(opath), path[3:])
    return os.path.join(opath, path).replace('\\', '/')


def app_pack(app, request, raise_ex=False, filenames=None):
    """Builds a w2p package for the application

    Args:
        app(str): application name
        request: the global request object
    Returns:
        filename of the w2p file or None on error

    """
    try:
        if filenames is None: app_cleanup(app, request)
        filename = apath('../deposit/web2py.app.%s.w2p' % app, request)
        w2p_pack(filename, apath(app, request), filenames=filenames)
        return filename
    except Exception, e:
        if raise_ex:
            raise
        return False


def app_pack_compiled(app, request, raise_ex=False):
    """Builds a w2p bytecode-compiled package for the application

    Args:
        app(str): application name
        request: the global request object

    Returns:
        filename of the w2p file or None on error

    """

    try:
        filename = apath('../deposit/%s.w2p' % app, request)
        w2p_pack(filename, apath(app, request), compiled=True)
        return filename
    except Exception, e:
        if raise_ex:
            raise
        return None


def app_cleanup(app, request):
    """Removes session, cache and error files

    Args:
        app(str): application name
        request: the global request object

    Returns:
        True if everything went ok, False otherwise

    """
    r = True

    # Remove error files
    path = apath('%s/errors/' % app, request)
    if os.path.exists(path):
        for f in os.listdir(path):
            try:
                if f[:1] != '.': os.unlink(os.path.join(path, f))
            except IOError:
                r = False

    # Remove session files
    path = apath('%s/sessions/' % app, request)
    if os.path.exists(path):
        for f in os.listdir(path):
            try:
                if f[:1] != '.': recursive_unlink(os.path.join(path, f))
            except (OSError, IOError):
                r = False

    # Remove cache files
    path = apath('%s/cache/' % app, request)
    CacheOnDisk(folder=path).clear()
    
    if os.path.exists(path):
        for f in os.listdir(path):
            try:
                if f[:1] != '.': recursive_unlink(os.path.join(path, f))
            except (OSError, IOError):
                r = False
    return r


def app_compile(app, request):
    """Compiles the application

    Args:
        app(str): application name
        request: the global request object

    Returns:
        None if everything went ok, traceback text if errors are found

    """
    from compileapp import compile_application, remove_compiled_application
    folder = apath(app, request)
    try:
        compile_application(folder)
        return None
    except (Exception, RestrictedError):
        tb = traceback.format_exc(sys.exc_info)
        remove_compiled_application(folder)
        return tb


def app_create(app, request, force=False, key=None, info=False):
    """Create a copy of welcome.w2p (scaffolding) app

    Args:
        app(str): application name
        request: the global request object

    """
    path = apath(app, request)
    if not os.path.exists(path):
        try:
            os.mkdir(path)
        except:
            if info:
                return False, traceback.format_exc(sys.exc_info)
            else:
                return False
    elif not force:
        if info:
            return False, "Application exists"
        else:
            return False
    try:
        w2p_unpack('welcome.w2p', path)
        for subfolder in [
            'models', 'views', 'controllers', 'databases',
            'modules', 'cron', 'errors', 'sessions', 'cache',
            'languages', 'static', 'private', 'uploads']:
            subpath = os.path.join(path, subfolder)
            if not os.path.exists(subpath):
                os.mkdir(subpath)
        db = os.path.join(path, 'models', 'db.py')
        if os.path.exists(db):
            data = read_file(db)
            data = data.replace('<your secret key>',
                                'sha512:' + (key or web2py_uuid()))
            write_file(db, data)
        if info:
            return True, None
        else:
            return True
    except:
        rmtree(path)
        if info:
            return False, traceback.format_exc(sys.exc_info)
        else:
            return False


def app_install(app, fobj, request, filename, overwrite=None):
    """Installs an application:

    - Identifies file type by filename
    - Writes `fobj` contents to the `../deposit/` folder
    - Calls `w2p_unpack()` to do the job.

    Args:
        app(str): new application name
        fobj(obj): file object containing the application to be installed
        request: the global request object
        filename(str): original filename of the `fobj`,
            required to determine extension
        overwrite(bool): force overwrite of existing application

    Returns:
        name of the file where app is temporarily stored or `None` on failure

    """
    did_mkdir = False
    if filename[-4:] == '.w2p':
        extension = 'w2p'
    elif filename[-7:] == '.tar.gz':
        extension = 'tar.gz'
    else:
        extension = 'tar'
    upname = apath('../deposit/%s.%s' % (app, extension), request)

    try:
        write_file(upname, fobj.read(), 'wb')
        path = apath(app, request)
        if not overwrite:
            os.mkdir(path)
            did_mkdir = True
        w2p_unpack(upname, path)
        if extension != 'tar':
            os.unlink(upname)
        fix_newlines(path)
        return upname
    except Exception:
        if did_mkdir:
            rmtree(path)
        return False


def app_uninstall(app, request):
    """Uninstalls the application.

    Args:
        app(str): application name
        request: the global request object

    Returns:
        `True` on success, `False` on failure

    """
    try:
        # Hey App, this is your end...
        path = apath(app, request)
        rmtree(path)
        return True
    except Exception:
        return False


def plugin_pack(app, plugin_name, request):
    """Builds a w2p package for the plugin

    Args:
        app(str): application name
        plugin_name(str): the name of the plugin without `plugin_` prefix
        request: the current request app

    Returns:
        filename of the w2p file or False on error

    """
    try:
        filename = apath(
            '../deposit/web2py.plugin.%s.w2p' % plugin_name, request)
        w2p_pack_plugin(filename, apath(app, request), plugin_name)
        return filename
    except Exception:
        return False


def plugin_install(app, fobj, request, filename):
    """Installs a plugin:

    - Identifies file type by filename
    - Writes `fobj` contents to the `../deposit/` folder
    - Calls `w2p_unpack_plugin()` to do the job.

    Args:
        app(str): new application name
        fobj: file object containing the application to be installed
        request: the global request object
        filename: original filename of the `fobj`,
            required to determine extension

    Returns:
        name of the file where plugin is temporarily stored
        or `False` on failure

    """
    upname = apath('../deposit/%s' % filename, request)

    try:
        write_file(upname, fobj.read(), 'wb')
        path = apath(app, request)
        w2p_unpack_plugin(upname, path)
        fix_newlines(path)
        return upname
    except Exception:
        os.unlink(upname)
        return False


def check_new_version(myversion, version_url):
    """Compares current web2py's version with the latest stable web2py version.

    Args:
        myversion: the current version as stored in file `web2py/VERSION`
        version_URL: the URL that contains the version
                     of the latest stable release

    Returns:
        tuple: state, version

        - state : `True` if upgrade available, `False` if current
          version is up-to-date, -1 on error
        - version : the most up-to-version available

    """
    try:
        from urllib import urlopen
        version = urlopen(version_url).read()
        pversion = parse_version(version)
        pmyversion = parse_version(myversion)
    except IOError:
        import traceback
        print traceback.format_exc()
        return -1, myversion

    if pversion[:3]+pversion[-6:] > pmyversion[:3]+pmyversion[-6:]:
        return True, version
    else:
        return False, version


def unzip(filename, dir, subfolder=''):
    """Unzips filename into dir (.zip only, no .gz etc)

    Args:
        filename(str): archive
        dir(str): destination
        subfolder(str): if != '' unzips only files in subfolder

    """
    filename = abspath(filename)
    if not zipfile.is_zipfile(filename):
        raise RuntimeError('Not a valid zipfile')
    zf = zipfile.ZipFile(filename)
    if not subfolder.endswith('/'):
        subfolder += '/'
    n = len(subfolder)
    for name in sorted(zf.namelist()):
        if not name.startswith(subfolder):
            continue
        #print name[n:]
        if name.endswith('/'):
            folder = os.path.join(dir, name[n:])
            if not os.path.exists(folder):
                os.mkdir(folder)
        else:
            write_file(os.path.join(dir, name[n:]), zf.read(name), 'wb')


def upgrade(request, url='http://web2py.com'):
    """Upgrades web2py (src, osx, win) if a new version is posted.
    It detects whether src, osx or win is running and downloads the right one

    Args:
        request: the current request object
            (required to determine version and path)
        url: the incomplete url where to locate the latest web2py
             (actual url is url+'/examples/static/web2py_(src|osx|win).zip')

    Returns
        tuple: completed, traceback

        - completed: True on success, False on failure
          (network problem or old version)
        - traceback: None on success, raised exception details on failure

    """
    web2py_version = request.env.web2py_version
    gluon_parent = request.env.gluon_parent
    if not gluon_parent.endswith('/'):
        gluon_parent += '/'
    (check, version) = check_new_version(web2py_version,
                                         url + '/examples/default/version')
    if not check:
        return False, 'Already latest version'
    if os.path.exists(os.path.join(gluon_parent, 'web2py.exe')):
        version_type = 'win'
        destination = gluon_parent
        subfolder = 'web2py/'
    elif gluon_parent.endswith('/Contents/Resources/'):
        version_type = 'osx'
        destination = gluon_parent[:-len('/Contents/Resources/')]
        subfolder = 'web2py/web2py.app/'
    else:
        version_type = 'src'
        destination = gluon_parent
        subfolder = 'web2py/'

    full_url = url + '/examples/static/web2py_%s.zip' % version_type
    filename = abspath('web2py_%s_downloaded.zip' % version_type)
    try:
        write_file(filename, urllib.urlopen(full_url).read(), 'wb')
    except Exception, e:
        return False, e
    try:
        unzip(filename, destination, subfolder)
        return True, None
    except Exception, e:
        return False, e


def add_path_first(path):
    sys.path = [path] + [p for p in sys.path if (
        not p == path and not p == (path + '/'))]
    if not global_settings.web2py_runtime_gae:
        site.addsitedir(path)


def create_missing_folders():
    if not global_settings.web2py_runtime_gae:
        for path in ('applications', 'deposit', 'site-packages', 'logs'):
            path = abspath(path, gluon=True)
            if not os.path.exists(path):
                os.mkdir(path)
    paths = (global_settings.gluon_parent, abspath(
        'site-packages', gluon=True), abspath('gluon', gluon=True), '')
    [add_path_first(path) for path in paths]


def create_missing_app_folders(request):
    if not global_settings.web2py_runtime_gae:
        if request.folder not in global_settings.app_folders:
            for subfolder in ('models', 'views', 'controllers', 'databases',
                              'modules', 'cron', 'errors', 'sessions',
                              'languages', 'static', 'private', 'uploads'):
                path = os.path.join(request.folder, subfolder)
                if not os.path.exists(path):
                    os.mkdir(path)
            global_settings.app_folders.add(request.folder)
