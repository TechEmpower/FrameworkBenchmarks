#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Functions required to execute app components
---------------------------------------------

Note:
    FOR INTERNAL USE ONLY
"""

import re
import fnmatch
import os
import copy
import random
import __builtin__
from gluon.storage import Storage, List
from gluon.template import parse_template
from gluon.restricted import restricted, compile2
from gluon.fileutils import mktree, listdir, read_file, write_file
from gluon.myregex import regex_expose, regex_longcomments
from gluon.languages import translator
from gluon.dal import DAL, Field
from pydal.base import BaseAdapter
from gluon.sqlhtml import SQLFORM, SQLTABLE
from gluon.cache import Cache
from gluon.globals import current, Response
from gluon import settings
from gluon.cfs import getcfs
from gluon import html
from gluon import validators
from gluon.http import HTTP, redirect
import marshal
import shutil
import imp
import logging
import types
logger = logging.getLogger("web2py")
from gluon import rewrite
from custom_import import custom_import_install

try:
    import py_compile
except:
    logger.warning('unable to import py_compile')

is_pypy = settings.global_settings.is_pypy
is_gae = settings.global_settings.web2py_runtime_gae
is_jython = settings.global_settings.is_jython

pjoin = os.path.join

TEST_CODE = \
    r"""
def _TEST():
    import doctest, sys, cStringIO, types, cgi, gluon.fileutils
    if not gluon.fileutils.check_credentials(request):
        raise HTTP(401, web2py_error='invalid credentials')
    stdout = sys.stdout
    html = '<h2>Testing controller "%s.py" ... done.</h2><br/>\n' \
        % request.controller
    for key in sorted([key for key in globals() if not key in __symbols__+['_TEST']]):
        eval_key = eval(key)
        if type(eval_key) == types.FunctionType:
            number_doctests = sum([len(ds.examples) for ds in doctest.DocTestFinder().find(eval_key)])
            if number_doctests>0:
                sys.stdout = cStringIO.StringIO()
                name = '%s/controllers/%s.py in %s.__doc__' \
                    % (request.folder, request.controller, key)
                doctest.run_docstring_examples(eval_key,
                    globals(), False, name=name)
                report = sys.stdout.getvalue().strip()
                if report:
                    pf = 'failed'
                else:
                    pf = 'passed'
                html += '<h3 class="%s">Function %s [%s]</h3>\n' \
                    % (pf, key, pf)
                if report:
                    html += CODE(report, language='web2py', \
                        link='/examples/global/vars/').xml()
                html += '<br/>\n'
            else:
                html += \
                    '<h3 class="nodoctests">Function %s [no doctests]</h3><br/>\n' \
                    % (key)
    response._vars = html
    sys.stdout = stdout
_TEST()
"""

CACHED_REGEXES = {}
CACHED_REGEXES_MAX_SIZE = 1000


def re_compile(regex):
    try:
        return CACHED_REGEXES[regex]
    except KeyError:
        if len(CACHED_REGEXES) >= CACHED_REGEXES_MAX_SIZE:
            CACHED_REGEXES.clear()
        compiled_regex = CACHED_REGEXES[regex] = re.compile(regex)
        return compiled_regex


class mybuiltin(object):
    """
    NOTE could simple use a dict and populate it,
    NOTE not sure if this changes things though if monkey patching import.....
    """
    #__builtins__
    def __getitem__(self, key):
        try:
            return getattr(__builtin__, key)
        except AttributeError:
            raise KeyError(key)

    def __setitem__(self, key, value):
        setattr(self, key, value)


def LOAD(c=None, f='index', args=None, vars=None,
         extension=None, target=None, ajax=False, ajax_trap=False,
         url=None, user_signature=False, timeout=None, times=1,
         content='loading...', post_vars=Storage(), **attr):
    """  LOADs a component into the action's document

    Args:
        c(str): controller
        f(str): function
        args(tuple or list): arguments
        vars(dict): vars
        extension(str): extension
        target(str): id of the target
        ajax(bool): True to enable AJAX bahaviour
        ajax_trap(bool): True if `ajax` is set to `True`, traps
            both links and forms "inside" the target
        url(str): overrides `c`,`f`,`args` and `vars`
        user_signature(bool): adds hmac signature to all links
            with a key that is different for every user
        timeout(int): in milliseconds, specifies the time to wait before
            starting the request or the frequency if times is greater than
            1 or "infinity"
        times(integer or str): how many times the component will be requested
            "infinity" or "continuous" are accepted to reload indefinitely the
            component
    """
    from html import TAG, DIV, URL, SCRIPT, XML
    if args is None:
        args = []
    vars = Storage(vars or {})
    target = target or 'c' + str(random.random())[2:]
    attr['_id'] = target
    request = current.request
    if '.' in f:
        f, extension = f.rsplit('.', 1)
    if url or ajax:
        url = url or URL(request.application, c, f, r=request,
                         args=args, vars=vars, extension=extension,
                         user_signature=user_signature)
        # timing options
        if isinstance(times, basestring):
            if times.upper() in ("INFINITY", "CONTINUOUS"):
                times = "Infinity"
            else:
                raise TypeError("Unsupported times argument %s" % times)
        elif isinstance(times, int):
            if times <= 0:
                raise ValueError("Times argument must be greater than zero, 'Infinity' or None")
        else:
            raise TypeError("Unsupported times argument type %s" % type(times))
        if timeout is not None:
            if not isinstance(timeout, (int, long)):
                raise ValueError("Timeout argument must be an integer or None")
            elif timeout <= 0:
                raise ValueError(
                    "Timeout argument must be greater than zero or None")
            statement = "$.web2py.component('%s','%s', %s, %s);" \
                % (url, target, timeout, times)
            attr['_data-w2p_timeout'] = timeout
            attr['_data-w2p_times'] = times
        else:
            statement = "$.web2py.component('%s','%s');" % (url, target)
        attr['_data-w2p_remote'] = url
        if not target is None:
            return DIV(content, **attr)

    else:
        if not isinstance(args, (list, tuple)):
            args = [args]
        c = c or request.controller
        other_request = Storage(request)
        other_request['env'] = Storage(request.env)
        other_request.controller = c
        other_request.function = f
        other_request.extension = extension or request.extension
        other_request.args = List(args)
        other_request.vars = vars
        other_request.get_vars = vars
        other_request.post_vars = post_vars
        other_response = Response()
        other_request.env.path_info = '/' + \
            '/'.join([request.application, c, f] +
                     map(str, other_request.args))
        other_request.env.query_string = \
            vars and URL(vars=vars).split('?')[1] or ''
        other_request.env.http_web2py_component_location = \
            request.env.path_info
        other_request.cid = target
        other_request.env.http_web2py_component_element = target
        other_request.restful = types.MethodType(request.restful.im_func, other_request) # A bit nasty but needed to use LOAD on action decorates with @request.restful()
        other_response.view = '%s/%s.%s' % (c, f, other_request.extension)

        other_environment = copy.copy(current.globalenv)  # NASTY

        other_response._view_environment = other_environment
        other_response.generic_patterns = \
            copy.copy(current.response.generic_patterns)
        other_environment['request'] = other_request
        other_environment['response'] = other_response

        ## some magic here because current are thread-locals

        original_request, current.request = current.request, other_request
        original_response, current.response = current.response, other_response
        page = run_controller_in(c, f, other_environment)
        if isinstance(page, dict):
            other_response._vars = page
            other_response._view_environment.update(page)
            run_view_in(other_response._view_environment)
            page = other_response.body.getvalue()
        current.request, current.response = original_request, original_response
        js = None
        if ajax_trap:
            link = URL(request.application, c, f, r=request,
                       args=args, vars=vars, extension=extension,
                       user_signature=user_signature)
            js = "$.web2py.trap_form('%s','%s');" % (link, target)
        script = js and SCRIPT(js, _type="text/javascript") or ''
        return TAG[''](DIV(XML(page), **attr), script)


class LoadFactory(object):
    """
    Attention: this helper is new and experimental
    """
    def __init__(self, environment):
        self.environment = environment

    def __call__(self, c=None, f='index', args=None, vars=None,
                 extension=None, target=None, ajax=False, ajax_trap=False,
                 url=None, user_signature=False, content='loading...', **attr):
        if args is None:
            args = []
        vars = Storage(vars or {})
        import globals
        target = target or 'c' + str(random.random())[2:]
        attr['_id'] = target
        request = self.environment['request']
        if '.' in f:
            f, extension = f.rsplit('.', 1)
        if url or ajax:
            url = url or html.URL(request.application, c, f, r=request,
                                  args=args, vars=vars, extension=extension,
                                  user_signature=user_signature)
            script = html.SCRIPT('$.web2py.component("%s","%s")' % (url, target),
                                 _type="text/javascript")
            return html.TAG[''](script, html.DIV(content, **attr))
        else:
            if not isinstance(args, (list, tuple)):
                args = [args]
            c = c or request.controller

            other_request = Storage(request)
            other_request['env'] = Storage(request.env)
            other_request.controller = c
            other_request.function = f
            other_request.extension = extension or request.extension
            other_request.args = List(args)
            other_request.vars = vars
            other_request.get_vars = vars
            other_request.post_vars = Storage()
            other_response = globals.Response()
            other_request.env.path_info = '/' + \
                '/'.join([request.application, c, f] +
                         map(str, other_request.args))
            other_request.env.query_string = \
                vars and html.URL(vars=vars).split('?')[1] or ''
            other_request.env.http_web2py_component_location = \
                request.env.path_info
            other_request.cid = target
            other_request.env.http_web2py_component_element = target
            other_response.view = '%s/%s.%s' % (c, f, other_request.extension)
            other_environment = copy.copy(self.environment)
            other_response._view_environment = other_environment
            other_response.generic_patterns = \
                copy.copy(current.response.generic_patterns)
            other_environment['request'] = other_request
            other_environment['response'] = other_response

            ## some magic here because current are thread-locals

            original_request, current.request = current.request, other_request
            original_response, current.response = current.response, other_response
            page = run_controller_in(c, f, other_environment)
            if isinstance(page, dict):
                other_response._vars = page
                other_response._view_environment.update(page)
                run_view_in(other_response._view_environment)
                page = other_response.body.getvalue()
            current.request, current.response = original_request, original_response
            js = None
            if ajax_trap:
                link = html.URL(request.application, c, f, r=request,
                                args=args, vars=vars, extension=extension,
                                user_signature=user_signature)
                js = "$.web2py.trap_form('%s','%s');" % (link, target)
            script = js and html.SCRIPT(js, _type="text/javascript") or ''
            return html.TAG[''](html.DIV(html.XML(page), **attr), script)


def local_import_aux(name, reload_force=False, app='welcome'):
    """
    In apps, instead of importing a local module
    (in applications/app/modules) with::

       import a.b.c as d

    you should do::

       d = local_import('a.b.c')

    or (to force a reload):

       d = local_import('a.b.c', reload=True)

    This prevents conflict between applications and un-necessary execs.
    It can be used to import any module, including regular Python modules.
    """
    items = name.replace('/', '.')
    name = "applications.%s.modules.%s" % (app, items)
    module = __import__(name)
    for item in name.split(".")[1:]:
        module = getattr(module, item)
    if reload_force:
        reload(module)
    return module


"""
OLD IMPLEMENTATION:
    items = name.replace('/','.').split('.')
    filename, modulepath = items[-1], pjoin(apath,'modules',*items[:-1])
    imp.acquire_lock()
    try:
        file=None
        (file,path,desc) = imp.find_module(filename,[modulepath]+sys.path)
        if not path in sys.modules or reload:
            if is_gae:
                module={}
                execfile(path,{},module)
                module=Storage(module)
            else:
                module = imp.load_module(path,file,path,desc)
            sys.modules[path] = module
        else:
            module = sys.modules[path]
    except Exception, e:
        module = None
    if file:
        file.close()
    imp.release_lock()
    if not module:
        raise ImportError, "cannot find module %s in %s" % (
            filename, modulepath)
    return module
"""

_base_environment_ = dict((k, getattr(html, k)) for k in html.__all__)
_base_environment_.update(
    (k, getattr(validators, k)) for k in validators.__all__)
_base_environment_['__builtins__'] = __builtins__
_base_environment_['HTTP'] = HTTP
_base_environment_['redirect'] = redirect
_base_environment_['DAL'] = DAL
_base_environment_['Field'] = Field
_base_environment_['SQLDB'] = DAL        # for backward compatibility
_base_environment_['SQLField'] = Field  # for backward compatibility
_base_environment_['SQLFORM'] = SQLFORM
_base_environment_['SQLTABLE'] = SQLTABLE
_base_environment_['LOAD'] = LOAD

def build_environment(request, response, session, store_current=True):
    """
    Build the environment dictionary into which web2py files are executed.
    """
    #h,v = html,validators
    environment = dict(_base_environment_)

    if not request.env:
        request.env = Storage()
    # Enable standard conditional models (i.e., /*.py, /[controller]/*.py, and
    # /[controller]/[function]/*.py)
    response.models_to_run = [
        r'^\w+\.py$',
        r'^%s/\w+\.py$' % request.controller,
        r'^%s/%s/\w+\.py$' % (request.controller, request.function)
        ]

    t = environment['T'] = translator(os.path.join(request.folder,'languages'),
                                      request.env.http_accept_language)
    c = environment['cache'] = Cache(request)

    if store_current:
        current.globalenv = environment
        current.request = request
        current.response = response
        current.session = session
        current.T = t
        current.cache = c

    global __builtins__
    if is_jython:  # jython hack
        __builtins__ = mybuiltin()
    elif is_pypy:  # apply the same hack to pypy too
        __builtins__ = mybuiltin()
    else:
        __builtins__['__import__'] = __builtin__.__import__  # WHY?
    environment['request'] = request
    environment['response'] = response
    environment['session'] = session
    environment['local_import'] = \
        lambda name, reload=False, app=request.application:\
        local_import_aux(name, reload, app)
    BaseAdapter.set_folder(pjoin(request.folder, 'databases'))
    response._view_environment = copy.copy(environment)
    custom_import_install()
    return environment


def save_pyc(filename):
    """
    Bytecode compiles the file `filename`
    """
    py_compile.compile(filename)


def read_pyc(filename):
    """
    Read the code inside a bytecode compiled file if the MAGIC number is
    compatible

    Returns:
        a code object
    """
    data = read_file(filename, 'rb')
    if not is_gae and data[:4] != imp.get_magic():
        raise SystemError('compiled code is incompatible')
    return marshal.loads(data[8:])


def compile_views(folder):
    """
    Compiles all the views in the application specified by `folder`
    """

    path = pjoin(folder, 'views')
    for fname in listdir(path, '^[\w/\-]+(\.\w+)*$'):
        try:
            data = parse_template(fname, path)
        except Exception, e:
            raise Exception("%s in %s" % (e, fname))
        filename = 'views.%s.py' % fname.replace(os.path.sep, '.')
        filename = pjoin(folder, 'compiled', filename)
        write_file(filename, data)
        save_pyc(filename)
        os.unlink(filename)


def compile_models(folder):
    """
    Compiles all the models in the application specified by `folder`
    """

    path = pjoin(folder, 'models')
    for fname in listdir(path, '.+\.py$'):
        data = read_file(pjoin(path, fname))
        modelfile = 'models.'+fname.replace(os.path.sep,'.')
        filename = pjoin(folder, 'compiled', modelfile)
        mktree(filename)
        write_file(filename, data)
        save_pyc(filename)
        os.unlink(filename)

def find_exposed_functions(data):
    data = regex_longcomments.sub('',data)
    return regex_expose.findall(data)

def compile_controllers(folder):
    """
    Compiles all the controllers in the application specified by `folder`
    """

    path = pjoin(folder, 'controllers')
    for fname in listdir(path, '.+\.py$'):
        ### why is this here? save_pyc(pjoin(path, file))
        data = read_file(pjoin(path, fname))
        exposed = find_exposed_functions(data)
        for function in exposed:
            command = data + "\nresponse._vars=response._caller(%s)\n" % \
                function
            filename = pjoin(folder, 'compiled',
                             'controllers.%s.%s.py' % (fname[:-3],function))
            write_file(filename, command)
            save_pyc(filename)
            os.unlink(filename)

def model_cmp(a, b, sep='.'):
    return cmp(a.count(sep), b.count(sep)) or cmp(a, b)

def model_cmp_sep(a, b, sep=os.path.sep):
    return model_cmp(a,b,sep)

def run_models_in(environment):
    """
    Runs all models (in the app specified by the current folder)
    It tries pre-compiled models first before compiling them.
    """

    folder = environment['request'].folder
    c = environment['request'].controller
    #f = environment['request'].function
    response = environment['response']

    path = pjoin(folder, 'models')
    cpath = pjoin(folder, 'compiled')
    compiled = os.path.exists(cpath)
    if compiled:
        models = sorted(listdir(cpath, '^models[_.][\w.]+\.pyc$', 0), model_cmp)
    else:
        models = sorted(listdir(path, '^\w+\.py$', 0, sort=False), model_cmp_sep)
    models_to_run = None
    for model in models:
        if response.models_to_run != models_to_run:
            regex = models_to_run = response.models_to_run[:]
            if isinstance(regex, list):
                regex = re_compile('|'.join(regex))
        if models_to_run:
            if compiled:
                n = len(cpath)+8
                fname = model[n:-4].replace('.','/')+'.py'
            else:
                n = len(path)+1
                fname = model[n:].replace(os.path.sep,'/')
            if not regex.search(fname) and c != 'appadmin':
                continue
            elif compiled:
                code = read_pyc(model)
            elif is_gae:
                code = getcfs(model, model,
                              lambda: compile2(read_file(model), model))
            else:
                code = getcfs(model, model, None)
            restricted(code, environment, layer=model)


def run_controller_in(controller, function, environment):
    """
    Runs the controller.function() (for the app specified by
    the current folder).
    It tries pre-compiled controller_function.pyc first before compiling it.
    """

    # if compiled should run compiled!
    folder = environment['request'].folder
    path = pjoin(folder, 'compiled')
    badc = 'invalid controller (%s/%s)' % (controller, function)
    badf = 'invalid function (%s/%s)' % (controller, function)
    if os.path.exists(path):
        filename = pjoin(path, 'controllers.%s.%s.pyc'
                         % (controller, function))
        if not os.path.exists(filename):
            ### for backward compatibility
            filename = pjoin(path, 'controllers_%s_%s.pyc'
                             % (controller, function))
            ### end for backward compatibility
            if not os.path.exists(filename):
                raise HTTP(404,
                           rewrite.THREAD_LOCAL.routes.error_message % badf,
                           web2py_error=badf)
        restricted(read_pyc(filename), environment, layer=filename)
    elif function == '_TEST':
        # TESTING: adjust the path to include site packages
        from settings import global_settings
        from admin import abspath, add_path_first
        paths = (global_settings.gluon_parent, abspath(
            'site-packages', gluon=True), abspath('gluon', gluon=True), '')
        [add_path_first(path) for path in paths]
        # TESTING END

        filename = pjoin(folder, 'controllers/%s.py'
                                 % controller)
        if not os.path.exists(filename):
            raise HTTP(404,
                       rewrite.THREAD_LOCAL.routes.error_message % badc,
                       web2py_error=badc)
        environment['__symbols__'] = environment.keys()
        code = read_file(filename)
        code += TEST_CODE
        restricted(code, environment, layer=filename)
    else:
        filename = pjoin(folder, 'controllers/%s.py'
                                 % controller)
        if not os.path.exists(filename):
            raise HTTP(404,
                       rewrite.THREAD_LOCAL.routes.error_message % badc,
                       web2py_error=badc)
        code = read_file(filename)
        exposed = find_exposed_functions(code)
        if not function in exposed:
            raise HTTP(404,
                       rewrite.THREAD_LOCAL.routes.error_message % badf,
                       web2py_error=badf)
        code = "%s\nresponse._vars=response._caller(%s)\n" % (code, function)
        if is_gae:
            layer = filename + ':' + function
            code = getcfs(layer, filename, lambda: compile2(code, layer))
        restricted(code, environment, filename)
    response = environment['response']
    vars = response._vars
    if response.postprocessing:
        vars = reduce(lambda vars, p: p(vars), response.postprocessing, vars)
    if isinstance(vars, unicode):
        vars = vars.encode('utf8')
    elif hasattr(vars, 'xml') and callable(vars.xml):
        vars = vars.xml()
    return vars


def run_view_in(environment):
    """
    Executes the view for the requested action.
    The view is the one specified in `response.view` or determined by the url
    or `view/generic.extension`
    It tries the pre-compiled views_controller_function.pyc before compiling it.
    """
    request = environment['request']
    response = environment['response']
    view = response.view
    folder = request.folder
    path = pjoin(folder, 'compiled')
    badv = 'invalid view (%s)' % view
    patterns = response.get('generic_patterns')
    if patterns:
        regex = re_compile('|'.join(map(fnmatch.translate, patterns)))
        short_action = '%(controller)s/%(function)s.%(extension)s' % request
        allow_generic = regex.search(short_action)
    else:
        allow_generic = False
    if not isinstance(view, str):
        ccode = parse_template(view, pjoin(folder, 'views'),
                               context=environment)
        restricted(ccode, environment, 'file stream')
    elif os.path.exists(path):
        x = view.replace('/', '.')
        files = ['views.%s.pyc' % x]
        if allow_generic:
            files.append('views.generic.%s.pyc' % request.extension)
        # for backward compatibility
        x = view.replace('/', '_')
        files.append('views_%s.pyc' % x)
        if allow_generic:
            files.append('views_generic.%s.pyc' % request.extension)
        if request.extension == 'html':
            files.append('views_%s.pyc' % x[:-5])
            if allow_generic:
                files.append('views_generic.pyc')
        # end backward compatibility code
        for f in files:
            filename = pjoin(path, f)
            if os.path.exists(filename):
                code = read_pyc(filename)
                restricted(code, environment, layer=filename)
                return
        raise HTTP(404,
                   rewrite.THREAD_LOCAL.routes.error_message % badv,
                   web2py_error=badv)
    else:
        filename = pjoin(folder, 'views', view)
        if not os.path.exists(filename) and allow_generic:
            view = 'generic.' + request.extension
            filename = pjoin(folder, 'views', view)
        if not os.path.exists(filename):
            raise HTTP(404,
                       rewrite.THREAD_LOCAL.routes.error_message % badv,
                       web2py_error=badv)
        layer = filename
        if is_gae:
            ccode = getcfs(layer, filename,
                           lambda: compile2(parse_template(view,
                                            pjoin(folder, 'views'),
                                            context=environment), layer))
        else:
            ccode = parse_template(view,
                                   pjoin(folder, 'views'),
                                   context=environment)
        restricted(ccode, environment, layer)


def remove_compiled_application(folder):
    """
    Deletes the folder `compiled` containing the compiled application.
    """
    try:
        shutil.rmtree(pjoin(folder, 'compiled'))
        path = pjoin(folder, 'controllers')
        for file in listdir(path, '.*\.pyc$', drop=False):
            os.unlink(file)
    except OSError:
        pass


def compile_application(folder):
    """
    Compiles all models, views, controller for the application in `folder`.
    """
    remove_compiled_application(folder)
    os.mkdir(pjoin(folder, 'compiled'))
    compile_models(folder)
    compile_controllers(folder)
    compile_views(folder)


def test():
    """
    Example::

        >>> import traceback, types
        >>> environment={'x':1}
        >>> open('a.py', 'w').write('print 1/x')
        >>> save_pyc('a.py')
        >>> os.unlink('a.py')
        >>> if type(read_pyc('a.pyc'))==types.CodeType: print 'code'
        code
        >>> exec read_pyc('a.pyc') in environment
        1
    """

    return


if __name__ == '__main__':
    import doctest
    doctest.testmod()
