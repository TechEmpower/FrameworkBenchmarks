#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Developed by Massimo Di Pierro <mdipierro@cs.depaul.edu>,
| limodou <limodou@gmail.com> and srackham <srackham@gmail.com>.
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Web2py environment in the shell
--------------------------------
"""

import os
import sys
import code
import logging
import types
import re
import optparse
import glob
import traceback
import gluon.fileutils as fileutils
from gluon.settings import global_settings
from gluon.utils import web2py_uuid
from gluon.compileapp import build_environment, read_pyc, run_models_in
from gluon.restricted import RestrictedError
from gluon.globals import Request, Response, Session
from gluon.storage import Storage, List
from gluon.admin import w2p_unpack
from pydal.base import BaseAdapter

logger = logging.getLogger("web2py")


def enable_autocomplete_and_history(adir, env):
    try:
        import rlcompleter
        import atexit
        import readline
    except ImportError:
        pass
    else:
        readline.parse_and_bind("bind ^I rl_complete"
                                if sys.platform == 'darwin'
                                else "tab: complete")
        history_file = os.path.join(adir, '.pythonhistory')
        try:
            readline.read_history_file(history_file)
        except IOError:
            open(history_file, 'a').close()
        atexit.register(readline.write_history_file, history_file)
        readline.set_completer(rlcompleter.Completer(env).complete)


def exec_environment(
    pyfile='',
    request=None,
    response=None,
    session=None,
):
    """Environment builder and module loader.

    Builds a web2py environment and optionally executes a Python file into
    the environment.

    A Storage dictionary containing the resulting environment is returned.
    The working directory must be web2py root -- this is the web2py default.

    """

    if request is None:
        request = Request({})
    if response is None:
        response = Response()
    if session is None:
        session = Session()

    if request.folder is None:
        mo = re.match(r'(|.*/)applications/(?P<appname>[^/]+)', pyfile)
        if mo:
            appname = mo.group('appname')
            request.folder = os.path.join('applications', appname)
        else:
            request.folder = ''
    env = build_environment(request, response, session, store_current=False)
    if pyfile:
        pycfile = pyfile + 'c'
        if os.path.isfile(pycfile):
            exec read_pyc(pycfile) in env
        else:
            execfile(pyfile, env)
    return Storage(env)


def env(
    a,
    import_models=False,
    c=None,
    f=None,
    dir='',
    extra_request={},
):
    """
    Returns web2py execution environment for application (a), controller (c),
    function (f).
    If import_models is True the exec all application models into the
    environment.

    extra_request allows you to pass along any extra variables to the request
    object before your models get executed. This was mainly done to support
    web2py_utils.test_runner, however you can use it with any wrapper scripts
    that need access to the web2py environment.
    """

    request = Request({})
    response = Response()
    session = Session()
    request.application = a

    # Populate the dummy environment with sensible defaults.

    if not dir:
        request.folder = os.path.join('applications', a)
    else:
        request.folder = dir
    request.controller = c or 'default'
    request.function = f or 'index'
    response.view = '%s/%s.html' % (request.controller,
                                    request.function)
    if global_settings.cmd_options:
        ip = global_settings.cmd_options.ip
        port = global_settings.cmd_options.port
    else:
        ip, port = '127.0.0.1', '8000'
    request.env.http_host = '%s:%s' % (ip, port)
    request.env.remote_addr = '127.0.0.1'
    request.env.web2py_runtime_gae = global_settings.web2py_runtime_gae

    for k, v in extra_request.items():
        request[k] = v

    path_info = '/%s/%s/%s' % (a, c, f)
    if request.args:
        path_info = '%s/%s' % (path_info, '/'.join(request.args))
    if request.vars:
        vars = ['%s=%s' % (k, v) if v else '%s' % k
                for (k, v) in request.vars.iteritems()]
        path_info = '%s?%s' % (path_info, '&'.join(vars))
    request.env.path_info = path_info

    # Monkey patch so credentials checks pass.

    def check_credentials(request, other_application='admin'):
        return True

    fileutils.check_credentials = check_credentials

    environment = build_environment(request, response, session)

    if import_models:
        try:
            run_models_in(environment)
        except RestrictedError, e:
            sys.stderr.write(e.traceback + '\n')
            sys.exit(1)

    environment['__name__'] = '__main__'
    return environment


def exec_pythonrc():
    pythonrc = os.environ.get('PYTHONSTARTUP')
    if pythonrc and os.path.isfile(pythonrc):
        def execfile_getlocals(file):
            execfile(file)
            return locals()
        try:
            return execfile_getlocals(pythonrc)
        except NameError:
            pass
    return dict()


def run(
    appname,
    plain=False,
    import_models=False,
    startfile=None,
    bpython=False,
    python_code=False,
    cronjob=False):
    """
    Start interactive shell or run Python script (startfile) in web2py
    controller environment. appname is formatted like:

    - a : web2py application name
    - a/c : exec the controller c into the application environment
    """

    (a, c, f, args, vars) = parse_path_info(appname, av=True)
    errmsg = 'invalid application name: %s' % appname
    if not a:
        die(errmsg)
    adir = os.path.join('applications', a)

    if not os.path.exists(adir):
        if sys.stdin and not sys.stdin.name == '/dev/null':
            confirm = raw_input(
                'application %s does not exist, create (y/n)?' % a)
        else:
            logging.warn('application does not exist and will not be created')
            return
        if confirm.lower() in ['y', 'yes']:

            os.mkdir(adir)
            w2p_unpack('welcome.w2p', adir)
            for subfolder in ['models', 'views', 'controllers', 'databases',
                              'modules', 'cron', 'errors', 'sessions',
                              'languages', 'static', 'private', 'uploads']:
                subpath = os.path.join(adir, subfolder)
                if not os.path.exists(subpath):
                    os.mkdir(subpath)
            db = os.path.join(adir, 'models/db.py')
            if os.path.exists(db):
                data = fileutils.read_file(db)
                data = data.replace(
                    '<your secret key>', 'sha512:' + web2py_uuid())
                fileutils.write_file(db, data)

    if c:
        import_models = True
    extra_request = {}
    if args:
        extra_request['args'] = args
    if vars:
        extra_request['vars'] = vars
    _env = env(a, c=c, f=f, import_models=import_models, extra_request=extra_request)
    if c:
        pyfile = os.path.join('applications', a, 'controllers', c + '.py')
        pycfile = os.path.join('applications', a, 'compiled',
                                 "controllers_%s_%s.pyc" % (c, f))
        if ((cronjob and os.path.isfile(pycfile))
            or not os.path.isfile(pyfile)):
            exec read_pyc(pycfile) in _env
        elif os.path.isfile(pyfile):
            execfile(pyfile, _env)
        else:
            die(errmsg)

    if f:
        exec ('print %s()' % f, _env)
        return

    _env.update(exec_pythonrc())
    if startfile:
        try:
            ccode = None
            if startfile.endswith('.pyc'):
                ccode = read_pyc(startfile)
                exec ccode in _env
            else:
                execfile(startfile, _env)

            if import_models:
                BaseAdapter.close_all_instances('commit')
        except Exception, e:
            print traceback.format_exc()
            if import_models:
                BaseAdapter.close_all_instances('rollback')
    elif python_code:
        try:
            exec(python_code, _env)
            if import_models:
                BaseAdapter.close_all_instances('commit')
        except Exception, e:
            print traceback.format_exc()
            if import_models:
                BaseAdapter.close_all_instances('rollback')
    else:
        if not plain:
            if bpython:
                try:
                    import bpython
                    bpython.embed(locals_=_env)
                    return
                except:
                    logger.warning(
                        'import bpython error; trying ipython...')
            else:
                try:
                    import IPython
                    if IPython.__version__ > '1.0.0':
                        IPython.start_ipython(user_ns=_env)
                        return
                    elif IPython.__version__ == '1.0.0':
                        from IPython.terminal.embed import InteractiveShellEmbed
                        shell = InteractiveShellEmbed(user_ns=_env)
                        shell()
                        return
                    elif IPython.__version__ >= '0.11':
                        from IPython.frontend.terminal.embed import InteractiveShellEmbed
                        shell = InteractiveShellEmbed(user_ns=_env)
                        shell()
                        return
                    else:
                        # following 2 lines fix a problem with
                        # IPython; thanks Michael Toomim
                        if '__builtins__' in _env:
                            del _env['__builtins__']
                        shell = IPython.Shell.IPShell(argv=[], user_ns=_env)
                        shell.mainloop()
                        return
                except:
                    logger.warning(
                        'import IPython error; use default python shell')
        enable_autocomplete_and_history(adir, _env)
        code.interact(local=_env)


def parse_path_info(path_info, av=False):
    """
    Parses path info formatted like a/c/f where c and f are optional
    and a leading `/` is accepted.
    Return tuple (a, c, f). If invalid path_info a is set to None.
    If c or f are omitted they are set to None.
    If av=True, parse args and vars
    """
    if av:
        vars = None
        if '?' in path_info:
            path_info, query = path_info.split('?', 2)
            vars = Storage()
            for var in query.split('&'):
                (var, val) = var.split('=', 2) if '=' in var else (var, None)
                vars[var] = val
        items = List(path_info.split('/'))
        args = List(items[3:]) if len(items) > 3 else None
        return (items(0), items(1), items(2), args, vars)

    mo = re.match(r'^/?(?P<a>\w+)(/(?P<c>\w+)(/(?P<f>\w+))?)?$',
                  path_info)
    if mo:
        return (mo.group('a'), mo.group('c'), mo.group('f'))
    else:
        return (None, None, None)


def die(msg):
    print >> sys.stderr, msg
    sys.exit(1)


def test(testpath, import_models=True, verbose=False):
    """
    Run doctests in web2py environment. testpath is formatted like:

    - a: tests all controllers in application a
    - a/c: tests controller c in application a
    - a/c/f  test function f in controller c, application a

    Where a, c and f are application, controller and function names
    respectively. If the testpath is a file name the file is tested.
    If a controller is specified models are executed by default.
    """

    import doctest
    if os.path.isfile(testpath):
        mo = re.match(r'(|.*/)applications/(?P<a>[^/]+)', testpath)
        if not mo:
            die('test file is not in application directory: %s'
                % testpath)
        a = mo.group('a')
        c = f = None
        files = [testpath]
    else:
        (a, c, f) = parse_path_info(testpath)
        errmsg = 'invalid test path: %s' % testpath
        if not a:
            die(errmsg)
        cdir = os.path.join('applications', a, 'controllers')
        if not os.path.isdir(cdir):
            die(errmsg)
        if c:
            cfile = os.path.join(cdir, c + '.py')
            if not os.path.isfile(cfile):
                die(errmsg)
            files = [cfile]
        else:
            files = glob.glob(os.path.join(cdir, '*.py'))
    for testfile in files:
        globs = env(a, import_models)
        ignores = globs.keys()
        execfile(testfile, globs)

        def doctest_object(name, obj):
            """doctest obj and enclosed methods and classes."""

            if type(obj) in (types.FunctionType, types.TypeType,
                             types.ClassType, types.MethodType,
                             types.UnboundMethodType):

                # Reload environment before each test.

                globs = env(a, c=c, f=f, import_models=import_models)
                execfile(testfile, globs)
                doctest.run_docstring_examples(
                    obj, globs=globs,
                    name='%s: %s' % (os.path.basename(testfile),
                                     name), verbose=verbose)
                if type(obj) in (types.TypeType, types.ClassType):
                    for attr_name in dir(obj):

                        # Execute . operator so decorators are executed.

                        o = eval('%s.%s' % (name, attr_name), globs)
                        doctest_object(attr_name, o)

        for (name, obj) in globs.items():
            if name not in ignores and (f is None or f == name):
                doctest_object(name, obj)


def get_usage():
    usage = """
  %prog [options] pythonfile
"""
    return usage


def execute_from_command_line(argv=None):
    if argv is None:
        argv = sys.argv

    parser = optparse.OptionParser(usage=get_usage())

    parser.add_option('-S', '--shell', dest='shell', metavar='APPNAME',
                      help='run web2py in interactive shell ' +
                      'or IPython(if installed) with specified appname')
    msg = 'run web2py in interactive shell or bpython (if installed) with'
    msg += ' specified appname (if app does not exist it will be created).'
    msg += '\n Use combined with --shell'
    parser.add_option(
        '-B',
        '--bpython',
        action='store_true',
        default=False,
        dest='bpython',
        help=msg,
    )
    parser.add_option(
        '-P',
        '--plain',
        action='store_true',
        default=False,
        dest='plain',
        help='only use plain python shell, should be used with --shell option',
    )
    parser.add_option(
        '-M',
        '--import_models',
        action='store_true',
        default=False,
        dest='import_models',
        help='auto import model files, default is False, ' +
        ' should be used with --shell option',
    )
    parser.add_option(
        '-R',
        '--run',
        dest='run',
        metavar='PYTHON_FILE',
        default='',
        help='run PYTHON_FILE in web2py environment, ' +
        'should be used with --shell option',
    )

    (options, args) = parser.parse_args(argv[1:])

    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(0)

    if len(args) > 0:
        startfile = args[0]
    else:
        startfile = ''
    run(options.shell, options.plain, startfile=startfile,
        bpython=options.bpython)


if __name__ == '__main__':
    execute_from_command_line()
