#!/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

gluon.rewrite parses incoming URLs and formats outgoing URLs for gluon.html.URL.

In addition, it rewrites both incoming and outgoing URLs based on the (optional) user-supplied routes.py,
which also allows for rewriting of certain error messages.

routes.py supports two styles of URL rewriting, depending on whether 'routers' is defined.
Refer to router.example.py and routes.example.py for additional documentation.

"""

import os
import re
import logging
import traceback
import threading
import urllib
from gluon.storage import Storage, List
from gluon.http import HTTP
from gluon.fileutils import abspath, read_file
from gluon.settings import global_settings

isdir = os.path.isdir
isfile = os.path.isfile
exists = os.path.exists
pjoin = os.path.join

logger = logging.getLogger('web2py.rewrite')
THREAD_LOCAL = threading.local()  # thread-local storage for routing params

regex_at = re.compile(r'(?<!\\)\$[a-zA-Z]\w*')
regex_anything = re.compile(r'(?<!\\)\$anything')
regex_redirect = re.compile(r'(\d+)->(.*)')
regex_full_url = re.compile(
    r'^(?P<scheme>http|https|HTTP|HTTPS)\://(?P<host>[^/]*)(?P<uri>.*)')
regex_version = re.compile(r'^(_[\d]+\.[\d]+\.[\d]+)$')

# pattern to find valid paths in url /application/controller/...
#   this could be:
#     for static pages:
#        /<b:application>/static/<x:file>
#     for dynamic pages:
#        /<a:application>[/<c:controller>[/<f:function>[.<e:ext>][/<s:args>]]]
#   application, controller, function and ext may only contain [a-zA-Z0-9_]
#   file and args may also contain '-', '=', '.' and '/'
#   apps in routes_apps_raw must parse raw_args into args

regex_url = re.compile('^/((?P<a>\w+)(/(?P<c>\w+)(/(?P<z>(?P<f>\w+)(\.(?P<e>[\w.]+))?(?P<s>.*)))?)?)?$')
regex_args = re.compile('[^\w/.@=-]')


def _router_default():
    """Returns new copy of default base router"""
    router = Storage(
        default_application='init',
        applications='ALL',
        default_controller='default',
        controllers='DEFAULT',
        default_function='index',
        functions=dict(),
        default_language=None,
        languages=None,
        root_static=['favicon.ico', 'robots.txt'],
        map_static=None,
        domains=None,
        exclusive_domain=False,
        map_hyphen=False,
        acfe_match=r'\w+$',                   # legal app/ctlr/fcn/ext
        #
        #  Implementation note:
        #  The file_match & args_match patterns use look-behind to avoid
        #  pathological backtracking from nested patterns.
        #
        file_match = r'([-+=@$%\w]|(?<=[-+=@$%\w])[./])*$', # legal static subpath
        args_match=r'([\w@ -]|(?<=[\w@ -])[.=])*$',         # legal arg in args
    )
    return router


def _params_default(app=None):
    """Returns a new copy of default parameters"""
    p = Storage()
    p.name = app or "BASE"
    p.default_application = app or "init"
    p.default_controller = "default"
    p.default_function = "index"
    p.routes_app = []
    p.routes_in = []
    p.routes_out = []
    p.routes_onerror = []
    p.routes_apps_raw = []
    p.error_handler = None
    p.error_message = '<html><body><h1>%s</h1></body></html>'
    p.error_message_ticket = \
        '<html><body><h1>Internal error</h1>Ticket issued: <a href="/admin/default/ticket/%(ticket)s" target="_blank">%(ticket)s</a></body><!-- this is junk text else IE does not display the page: ' + ('x' * 512) + ' //--></html>'
    p.routers = None
    p.logging = 'off'
    return p

params_apps = dict()
params = _params_default(app=None)  # regex rewrite parameters
THREAD_LOCAL.routes = params  # default to base regex rewrite parameters
routers = None


def log_rewrite(string):
    """Log rewrite activity under control of routes.py"""
    if params.logging == 'debug':   # catch common cases first
        logger.debug(string)
    elif params.logging == 'off' or not params.logging:
        pass
    elif params.logging == 'print':
        print string
    elif params.logging == 'info':
        logger.info(string)
    elif params.logging == 'warning':
        logger.warning(string)
    elif params.logging == 'error':
        logger.error(string)
    elif params.logging == 'critical':
        logger.critical(string)
    else:
        logger.debug(string)

ROUTER_KEYS = set(
    ('default_application', 'applications',
     'default_controller', 'controllers',
     'default_function', 'functions',
     'default_language', 'languages',
     'domain', 'domains', 'root_static', 'path_prefix',
     'exclusive_domain', 'map_hyphen', 'map_static',
     'acfe_match', 'file_match', 'args_match'))

ROUTER_BASE_KEYS = set(
    ('applications', 'default_application',
     'domains', 'path_prefix'))

#  The external interface to rewrite consists of:
#
#  load: load routing configuration file(s)
#  url_in: parse and rewrite incoming URL
#  url_out: assemble and rewrite outgoing URL
#
#  THREAD_LOCAL.routes.default_application
#  THREAD_LOCAL.routes.error_message
#  THREAD_LOCAL.routes.error_message_ticket
#  THREAD_LOCAL.routes.try_redirect_on_error
#  THREAD_LOCAL.routes.error_handler
#
#  filter_url: helper for doctest & unittest
#  filter_err: helper for doctest & unittest
#  regex_filter_out: doctest


def fixup_missing_path_info(environ):
    eget = environ.get
    path_info = eget('PATH_INFO')
    request_uri = eget('REQUEST_URI')
    if not path_info and request_uri:
        # for fcgi, get path_info and
        # query_string from request_uri
        items = request_uri.split('?')
        path_info = environ['PATH_INFO'] = items[0]
        environ['QUERY_STRING'] = items[1] if len(items) > 1 else ''
    elif not request_uri:
        query_string = eget('QUERY_STRING')
        if query_string:
            environ['REQUEST_URI'] = '%s?%s' % (path_info, query_string)
        else:
            environ['REQUEST_URI'] = path_info
    if not eget('HTTP_HOST'):
        environ['HTTP_HOST'] = \
            '%s:%s' % (eget('SERVER_NAME'), eget('SERVER_PORT'))


def url_in(request, environ):
    """Parses and rewrites incoming URL"""
    if routers:
        return map_url_in(request, environ)
    return regex_url_in(request, environ)


def url_out(request, environ, application, controller, function,
            args, other, scheme, host, port, language=None):
    """Assembles and rewrites outgoing URL"""
    if routers:
        acf = map_url_out(request, environ, application, controller,
                          function, args, other, scheme, host, port, language)
        url = '%s%s' % (acf, other)
    else:
        url = '/%s/%s/%s%s' % (application, controller, function, other)
        url = regex_filter_out(url, environ)
    #
    #  fill in scheme and host if absolute URL is requested
    #  scheme can be a string, eg 'http', 'https', 'ws', 'wss'
    #
    if host is True or (host is None and (scheme or port is not None)):
        host = request.env.http_host
    if not scheme or scheme is True:
        scheme = request.env.get('wsgi_url_scheme', 'http').lower() \
            if request else 'http'
    if host:
        host_port = host if not port else host.split(':', 1)[0] + ':%s' % port
        url = '%s://%s%s' % (scheme, host_port, url)
    return url


def try_rewrite_on_error(http_response, request, environ, ticket=None):
    """
    Called from main.wsgibase to rewrite the http response.
    """
    status = int(str(http_response.status).split()[0])
    if status >= 399 and THREAD_LOCAL.routes.routes_onerror:
        keys = set(('%s/%s' % (request.application, status),
                    '%s/*' % (request.application),
                    '*/%s' % (status),
                    '*/*'))
        for (key, uri) in THREAD_LOCAL.routes.routes_onerror:
            if key in keys:
                if uri == '!':
                    # do nothing!
                    return http_response, environ
                elif '?' in uri:
                    path_info, query_string = uri.split('?', 1)
                    query_string += '&'
                else:
                    path_info, query_string = uri, ''
                query_string += \
                    'code=%s&ticket=%s&requested_uri=%s&request_url=%s' % \
                    (status, ticket, urllib.quote_plus(
                        request.env.request_uri), request.url)
                if uri.startswith('http://') or uri.startswith('https://'):
                    # make up a response
                    url = path_info + '?' + query_string
                    message = 'You are being redirected <a href="%s">here</a>'
                    return HTTP(303, message % url, Location=url), environ
                elif not environ.get('__ROUTES_ONERROR__', False):
                    # wsgibase will be called recursively with
                    # the routes_onerror path.
                    environ['__ROUTES_ONERROR__'] = True # limit recursion
                    path_info = '/' + path_info.lstrip('/')
                    environ['PATH_INFO'] = path_info
                    environ['QUERY_STRING'] = query_string
                    environ['WEB2PY_STATUS_CODE'] = status
                    return None, environ
    # do nothing!
    return http_response, environ


def try_redirect_on_error(http_object, request, ticket=None):
    """Called from main.wsgibase to rewrite the http response"""
    status = int(str(http_object.status).split()[0])
    if status > 399 and THREAD_LOCAL.routes.routes_onerror:
        keys = set(('%s/%s' % (request.application, status),
                    '%s/*' % (request.application),
                    '*/%s' % (status),
                    '*/*'))
        for (key, redir) in THREAD_LOCAL.routes.routes_onerror:
            if key in keys:
                if redir == '!':
                    break
                elif '?' in redir:
                    url = '%s&code=%s&ticket=%s&requested_uri=%s&request_url=%s' % \
                        (redir, status, ticket,
                         urllib.quote_plus(request.env.request_uri),
                         request.url)
                else:
                    url = '%s?code=%s&ticket=%s&requested_uri=%s&request_url=%s' % \
                        (redir, status, ticket,
                         urllib.quote_plus(request.env.request_uri),
                         request.url)
                return HTTP(303, 'You are being redirected <a href="%s">here</a>' % url, Location=url)
    return http_object


def load(routes='routes.py', app=None, data=None, rdict=None):
    """
    load: read (if file) and parse routes
    store results in params
    (called from main.py at web2py initialization time)
    If data is present, it's used instead of the routes.py contents.
    If rdict is present, it must be a dict to be used for routers (unit test)
    """
    global params
    global routers
    if app is None:
        # reinitialize
        global params_apps
        params_apps = dict()
        params = _params_default(app=None)  # regex rewrite parameters
        THREAD_LOCAL.routes = params              # default to base regex rewrite parameters
        routers = None

    if isinstance(rdict, dict):
        symbols = dict(routers=rdict)
        path = 'rdict'
    else:
        if data is not None:
            path = 'routes'
        else:
            if app is None:
                path = abspath(routes)
            else:
                path = abspath('applications', app, routes)
            if not exists(path):
                return
            data = read_file(path).replace('\r\n', '\n')

        symbols = dict(app=app)
        try:
            exec (data + '\n') in symbols
        except SyntaxError, e:
            logger.error(
                '%s has a syntax error and will not be loaded\n' % path
                + traceback.format_exc())
            raise e

    p = _params_default(app)

    for sym in ('routes_app', 'routes_in', 'routes_out'):
        if sym in symbols:
            for items in symbols[sym]:
                p[sym].append(compile_regex(*items))
    for sym in ('routes_onerror', 'routes_apps_raw',
                'error_handler', 'error_message', 'error_message_ticket',
                'default_application', 'default_controller', 'default_function',
                'logging'):
        if sym in symbols:
            p[sym] = symbols[sym]
    if 'routers' in symbols:
        p.routers = Storage(symbols['routers'])
        for key in p.routers:
            if isinstance(p.routers[key], dict):
                p.routers[key] = Storage(p.routers[key])

    if app is None:
        params = p                  # install base rewrite parameters
        THREAD_LOCAL.routes = params      # install default as current routes
        #
        #  create the BASE router if routers in use
        #
        routers = params.routers    # establish routers if present
        if isinstance(routers, dict):
            routers = Storage(routers)
        if routers is not None:
            router = _router_default()
            if routers.BASE:
                router.update(routers.BASE)
            routers.BASE = router

        #  scan each app in applications/
        #    create a router, if routers are in use
        #    parse the app-specific routes.py if present
        #
        all_apps = []
        apppath = abspath('applications')
        for appname in os.listdir(apppath):
            if not appname.startswith('.') and \
                    isdir(abspath(apppath, appname)) and \
                    isdir(abspath(apppath, appname, 'controllers')):
                all_apps.append(appname)
                if routers:
                    router = Storage(routers.BASE)   # new copy
                    if appname in routers:
                        for key in routers[appname].keys():
                            if key in ROUTER_BASE_KEYS:
                                raise SyntaxError("BASE-only key '%s' in router '%s'" % (key, appname))
                        router.update(routers[appname])
                    routers[appname] = router
                if exists(abspath('applications', appname, routes)):
                    load(routes, appname)

        if routers:
            load_routers(all_apps)

    else:  # app
        params_apps[app] = p
        if routers and p.routers:
            if app in p.routers:
                routers[app].update(p.routers[app])

    log_rewrite('URL rewrite is on. configuration in %s' % path)


def compile_regex(k, v, env=None):
    """
    Preprocess and compile the regular expressions in routes_app/in/out
    The resulting regex will match a pattern of the form::

        [remote address]:[protocol]://[host]:[method] [path]

    We allow abbreviated regexes on input; here we try to complete them.
    """
    k0 = k  # original k for error reporting
    # bracket regex in ^...$ if not already done
    if not k[0] == '^':
        k = '^%s' % k
    if not k[-1] == '$':
        k = '%s$' % k
    # if there are no :-separated parts, prepend a catch-all for the IP address
    if k.find(':') < 0:
        # k = '^.*?:%s' % k[1:]
        k = '^.*?:https?://[^:/]+:[a-z]+ %s' % k[1:]
    # if there's no ://, provide a catch-all for the protocol, host & method
    if k.find('://') < 0:
        i = k.find(':/')
        if i < 0:
            raise SyntaxError("routes pattern syntax error: path needs leading '/' [%s]" % k0)
        k = r'%s:https?://[^:/]+:[a-z]+ %s' % (k[:i], k[i + 1:])
    # $anything -> ?P<anything>.*
    for item in regex_anything.findall(k):
        k = k.replace(item, '(?P<anything>.*)')
    # $a (etc) -> ?P<a>\w+
    for item in regex_at.findall(k):
        k = k.replace(item, r'(?P<%s>\w+)' % item[1:])
    # same for replacement pattern, but with \g
    for item in regex_at.findall(v):
        v = v.replace(item, r'\g<%s>' % item[1:])
    return (re.compile(k, re.DOTALL), v, env or {})


def load_routers(all_apps):
    """Load-time post-processing of routers"""

    for app in routers:
        # initialize apps with routers that aren't present,
        # on behalf of unit tests
        if app not in all_apps:
            all_apps.append(app)
            router = Storage(routers.BASE)   # new copy
            if app != 'BASE':
                keys = set(routers[app]).intersection(ROUTER_BASE_KEYS)
                if keys:
                    raise SyntaxError("BASE-only key(s) %s in router '%s'" % (
                        tuple(keys), app))
            router.update(routers[app])
            routers[app] = router
        router = routers[app]
        keys = set(router).difference(ROUTER_KEYS)
        if keys:
            raise SyntaxError("unknown key(s) %s in router '%s'" % (
                tuple(keys), app))
        if not router.controllers:
            router.controllers = set()
        elif not isinstance(router.controllers, str):
            router.controllers = set(router.controllers)
        if router.languages:
            router.languages = set(router.languages)
        else:
            router.languages = set()
        if router.functions:
            if isinstance(router.functions, (set, tuple, list)):
                functions = set(router.functions)
                if isinstance(router.default_function, str):
                    functions.add(
                        router.default_function)  # legacy compatibility
                router.functions = {router.default_controller: functions}
            for controller in router.functions:
                router.functions[controller] = set(
                    router.functions[controller])
        else:
            router.functions = dict()
        if app != 'BASE':
            for base_only in ROUTER_BASE_KEYS:
                router.pop(base_only, None)
            if 'domain' in router:
                routers.BASE.domains[router.domain] = app
            if isinstance(router.controllers, str) and router.controllers == 'DEFAULT':
                router.controllers = set()
                if isdir(abspath('applications', app)):
                    cpath = abspath('applications', app, 'controllers')
                    for cname in os.listdir(cpath):
                        if isfile(abspath(cpath, cname)) and cname.endswith('.py'):
                            router.controllers.add(cname[:-3])
            if router.controllers:
                router.controllers.add('static')
                router.controllers.add(router.default_controller)

    if isinstance(routers.BASE.applications, str) and routers.BASE.applications == 'ALL':
        routers.BASE.applications = list(all_apps)
    if routers.BASE.applications:
        routers.BASE.applications = set(routers.BASE.applications)
    else:
        routers.BASE.applications = set()

    for app in routers.keys():
        # set router name
        router = routers[app]
        router.name = app
        # compile URL validation patterns
        router._acfe_match = re.compile(router.acfe_match)
        router._file_match = re.compile(router.file_match)
        if router.args_match:
            router._args_match = re.compile(router.args_match)
        # convert path_prefix to a list of path elements
        if router.path_prefix:
            if isinstance(router.path_prefix, str):
                router.path_prefix = router.path_prefix.strip('/').split('/')

    #  rewrite BASE.domains as tuples
    #
    #      key:   'domain[:port]' -> (domain, port)
    #      value: 'application[/controller] -> (application, controller)
    #      (port and controller may be None)
    #
    domains = dict()
    if routers.BASE.domains:
        for (d, a) in routers.BASE.domains.iteritems():
            (domain, app) = (d.strip(':'), a.strip('/'))
            if ':' in domain:
                (domain, port) = domain.split(':')
            else:
                port = None
            if '/' in app:
                (app, ctlr) = app.split('/', 1)
            else:
                ctlr = None
            if ctlr and '/' in ctlr:
                (ctlr, fcn) = ctlr.split('/')
            else:
                fcn = None
            if app not in all_apps and app not in routers:
                raise SyntaxError("unknown app '%s' in domains" % app)
            domains[(domain, port)] = (app, ctlr, fcn)
    routers.BASE.domains = domains


def regex_uri(e, regexes, tag, default=None):
    """Filters incoming URI against a list of regexes"""
    path = e['PATH_INFO']
    host = e.get('HTTP_HOST', e.get('SERVER_NAME', 'localhost')).lower()
    i = host.find(':')
    if i > 0:
        host = host[:i]
    key = '%s:%s://%s:%s %s' % \
        (e.get('REMOTE_ADDR', 'localhost'),
         e.get('wsgi.url_scheme', 'http').lower(), host,
         e.get('REQUEST_METHOD', 'get').lower(), path)
    for (regex, value, custom_env) in regexes:
        if regex.match(key):
            e.update(custom_env)
            rewritten = regex.sub(value, key)
            log_rewrite('%s: [%s] [%s] -> %s' % (tag, key, value, rewritten))
            return rewritten
    log_rewrite('%s: [%s] -> %s (not rewritten)' % (tag, key, default))
    return default


def regex_select(env=None, app=None, request=None):
    """
    Selects a set of regex rewrite params for the current request
    """
    if app:
        THREAD_LOCAL.routes = params_apps.get(app, params)
    elif env and params.routes_app:
        if routers:
            map_url_in(request, env, app=True)
        else:
            app = regex_uri(env, params.routes_app, "routes_app")
            THREAD_LOCAL.routes = params_apps.get(app, params)
    else:
        THREAD_LOCAL.routes = params  # default to base rewrite parameters
    log_rewrite("select routing parameters: %s" % THREAD_LOCAL.routes.name)
    return app  # for doctest


def regex_filter_in(e):
    """Regex rewrite incoming URL"""
    routes = THREAD_LOCAL.routes
    query = e.get('QUERY_STRING', None)
    e['WEB2PY_ORIGINAL_URI'] = e['PATH_INFO'] + (query and ('?' + query) or '')
    if routes.routes_in:
        path = regex_uri(e, routes.routes_in,
                         "routes_in", e['PATH_INFO'])
        rmatch = regex_redirect.match(path)
        if rmatch:
            raise HTTP(int(rmatch.group(1)), location=rmatch.group(2))
        items = path.split('?', 1)
        e['PATH_INFO'] = items[0]
        if len(items) > 1:
            if query:
                query = items[1] + '&' + query
            else:
                query = items[1]
            e['QUERY_STRING'] = query
    e['REQUEST_URI'] = e['PATH_INFO'] + (query and ('?' + query) or '')
    return e


def sluggify(key):
    return key.lower().replace('.', '_')


def invalid_url(routes):
    raise HTTP(400,
               routes.error_message % 'invalid request',
               web2py_error='invalid path')


def regex_url_in(request, environ):
    """Rewrites and parses incoming URL"""

    # ##################################################
    # select application
    # rewrite URL if routes_in is defined
    # update request.env
    # ##################################################

    regex_select(env=environ, request=request)
    routes = THREAD_LOCAL.routes
    if routes.routes_in:
        environ = regex_filter_in(environ)
    request.env.update(
        (k.lower().replace('.', '_'), v) for k, v in environ.iteritems())

    # ##################################################
    # serve if a static file
    # ##################################################

    path = urllib.unquote(request.env.path_info) or '/'
    path = path.replace('\\', '/')
    if path.endswith('/') and len(path) > 1:
        path = path[:-1]
    match = regex_url.match(path)
    if not match:
        invalid_url(routes)
    request.raw_args = (match.group('s') or '')
    if request.raw_args.startswith('/'):
        request.raw_args = request.raw_args[1:]
    if match.group('c') == 'static':
        application = match.group('a')
        version, filename = None, match.group('z').replace(' ', '_')
        if not filename:
            raise HTTP(404)
        items = filename.split('/', 1)
        if regex_version.match(items[0]):
            version, filename = items
        static_folder = pjoin(request.env.applications_parent,
                              'applications', application, 'static')
        static_file = os.path.abspath(pjoin(static_folder, filename))
        if not static_file.startswith(static_folder):
            invalid_url(routes)
        return (static_file, version, environ)
    else:
        # ##################################################
        # parse application, controller and function
        # ##################################################
        request.application = match.group('a') or routes.default_application
        request.controller = match.group('c') or routes.default_controller
        request.function = match.group('f') or routes.default_function
        request.raw_extension = match.group('e')
        request.extension = request.raw_extension or 'html'
        if request.application in routes.routes_apps_raw:
            # application is responsible for parsing args
            request.args = None
        elif request.raw_args:
            args = regex_args.sub('_', request.raw_args)
            request.args = List(args.split('/'))
        else:
            request.args = List([])
    return (None, None, environ)


def regex_filter_out(url, e=None):
    """Regex rewrite outgoing URL"""
    if not hasattr(THREAD_LOCAL, 'routes'):
        regex_select()    # ensure routes is set (for application threads)
    routes = THREAD_LOCAL.routes
    if routers:
        return url  # already filtered
    if routes.routes_out:
        items = url.split('?', 1)
        if e:
            host = e.get('http_host', 'localhost').lower()
            i = host.find(':')
            if i > 0:
                host = host[:i]
            items[0] = '%s:%s://%s:%s %s' % \
                (e.get('remote_addr', ''),
                 e.get('wsgi_url_scheme', 'http').lower(), host,
                 e.get('request_method', 'get').lower(), items[0])
        else:
            items[0] = ':http://localhost:get %s' % items[0]
        for (regex, value, tmp) in routes.routes_out:
            if regex.match(items[0]):
                rewritten = '?'.join([regex.sub(value, items[0])] + items[1:])
                log_rewrite('routes_out: [%s] -> %s' % (url, rewritten))
                return rewritten
    log_rewrite('routes_out: [%s] not rewritten' % url)
    return url


def filter_url(url, method='get', remote='0.0.0.0',
               out=False, app=False, lang=None,
               domain=(None, None), env=False, scheme=None,
               host=None, port=None, language=None):
    """
    doctest/unittest interface to regex_filter_in() and regex_filter_out()
    """
    match = regex_full_url.match(url)
    urlscheme = match.group('scheme').lower()
    urlhost = match.group('host').lower()
    uri = match.group('uri')
    k = uri.find('?')
    if k < 0:
        k = len(uri)
    if isinstance(domain, str):
        domain = (domain, None)
    (path_info, query_string) = (uri[:k], uri[k + 1:])
    path_info = urllib.unquote(path_info)   # simulate server
    e = {
        'REMOTE_ADDR': remote,
        'REQUEST_METHOD': method,
        'wsgi.url_scheme': urlscheme,
        'HTTP_HOST': urlhost,
        'REQUEST_URI': uri,
        'PATH_INFO': path_info,
        'QUERY_STRING': query_string,
        #for filter_out request.env use lowercase
        'remote_addr': remote,
        'request_method': method,
        'wsgi_url_scheme': urlscheme,
        'http_host': urlhost
    }

    request = Storage()
    e["applications_parent"] = global_settings.applications_parent
    request.env = Storage(e)
    request.uri_language = lang

    #  determine application only
    #
    if app:
        if routers:
            return map_url_in(request, e, app=True)
        return regex_select(e)

    #  rewrite outbound URL
    #
    if out:
        (request.env.domain_application,
         request.env.domain_controller) = domain
        items = path_info.lstrip('/').split('/')
        if items[-1] == '':
            items.pop()  # adjust trailing empty args
        assert len(items) >= 3, "at least /a/c/f is required"
        a = items.pop(0)
        c = items.pop(0)
        f = items.pop(0)
        if not routers:
            return regex_filter_out(uri, e)
        acf = map_url_out(
            request, None, a, c, f, items, None, scheme, host, port, language=language)
        if items:
            url = '%s/%s' % (acf, '/'.join(items))
            if items[-1] == '':
                url += '/'
        else:
            url = acf
        if query_string:
            url += '?' + query_string
        return url

    #  rewrite inbound URL
    #
    (static, version, e) = url_in(request, e)
    if static:
        return static
    result = "/%s/%s/%s" % (
        request.application, request.controller, request.function)
    if request.extension and request.extension != 'html':
        result += ".%s" % request.extension
    if request.args:
        result += " %s" % request.args
    if e['QUERY_STRING']:
        result += " ?%s" % e['QUERY_STRING']
    if request.uri_language:
        result += " (%s)" % request.uri_language
    if env:
        return request.env
    return result


def filter_err(status, application='app', ticket='tkt'):
    """doctest/unittest interface to routes_onerror"""
    routes = THREAD_LOCAL.routes
    if status > 399 and routes.routes_onerror:
        keys = set(('%s/%s' % (application, status),
                    '%s/*' % (application),
                    '*/%s' % (status),
                    '*/*'))
        for (key, redir) in routes.routes_onerror:
            if key in keys:
                if redir == '!':
                    break
                elif '?' in redir:
                    url = redir + '&' + 'code=%s&ticket=%s' % (status, ticket)
                else:
                    url = redir + '?' + 'code=%s&ticket=%s' % (status, ticket)
                return url  # redirection
    return status  # no action

#  router support
#


class MapUrlIn(object):
    """Logic for mapping incoming URLs"""

    def __init__(self, request=None, env=None):
        """Initializes a map-in object"""
        self.request = request
        self.env = env

        self.router = None
        self.application = None
        self.language = None
        self.controller = None
        self.function = None
        self.extension = 'html'

        self.controllers = set()
        self.functions = dict()
        self.languages = set()
        self.default_language = None
        self.map_hyphen = False
        self.exclusive_domain = False

        path = self.env['PATH_INFO']
        self.query = self.env.get('QUERY_STRING', None)
        path = path.lstrip('/')
        self.env['PATH_INFO'] = '/' + path
        self.env['WEB2PY_ORIGINAL_URI'] = self.env['PATH_INFO'] + (
            self.query and ('?' + self.query) or '')

        # to handle empty args, strip exactly one trailing slash, if present
        # .../arg1// represents one trailing empty arg
        #
        if path.endswith('/'):
            path = path[:-1]
        self.args = List(path and path.split('/') or [])

        # see http://www.python.org/dev/peps/pep-3333/#url-reconstruction for URL composition
        self.remote_addr = self.env.get('REMOTE_ADDR', 'localhost')
        self.scheme = self.env.get('wsgi.url_scheme', 'http').lower()
        self.method = self.env.get('REQUEST_METHOD', 'get').lower()
        (self.host, self.port) = (self.env.get('HTTP_HOST'), None)
        if not self.host:
            (self.host, self.port) = (
                self.env.get('SERVER_NAME'), self.env.get('SERVER_PORT'))
        if not self.host:
            (self.host, self.port) = ('localhost', '80')
        if ':' in self.host:
            (self.host, self.port) = self.host.rsplit(':', 1)  # for ipv6 support
        if not self.port:
            self.port = '443' if self.scheme == 'https' else '80'

    def map_prefix(self):
        """Strips path prefix, if present in its entirety"""
        prefix = routers.BASE.path_prefix
        if prefix:
            prefixlen = len(prefix)
            if prefixlen > len(self.args):
                return
            for i in xrange(prefixlen):
                if prefix[i] != self.args[i]:
                    return  # prefix didn't match
            self.args = List(self.args[prefixlen:])  # strip the prefix

    def map_app(self):
        """Determines application name"""
        base = routers.BASE  # base router
        self.domain_application = None
        self.domain_controller = None
        self.domain_function = None
        self.map_hyphen = base.map_hyphen
        arg0 = self.harg0
        if not base.exclusive_domain and base.applications and arg0 in base.applications:
            self.application = arg0
        elif not base.exclusive_domain and arg0 and not base.applications:
            self.application = arg0
        elif (self.host, self.port) in base.domains:
            (self.application, self.domain_controller,
             self.domain_function) = base.domains[(self.host, self.port)]
            self.env['domain_application'] = self.application
            self.env['domain_controller'] = self.domain_controller
            self.env['domain_function'] = self.domain_function
        elif (self.host, None) in base.domains:
            (self.application, self.domain_controller,
             self.domain_function) = base.domains[(self.host, None)]
            self.env['domain_application'] = self.application
            self.env['domain_controller'] = self.domain_controller
            self.env['domain_function'] = self.domain_function
        elif base.applications and arg0 in base.applications:
            self.application = arg0
        elif arg0 and not base.applications:
            self.application = arg0
        else:
            self.application = base.default_application or ''
        self.pop_arg_if(self.application == arg0)

        if not base._acfe_match.match(self.application):
            raise HTTP(
                400, THREAD_LOCAL.routes.error_message % 'invalid request',
                web2py_error="invalid application: '%s'" % self.application)

        if self.application not in routers and \
                (self.application != THREAD_LOCAL.routes.default_application or self.application == 'welcome'):
            raise HTTP(
                400, THREAD_LOCAL.routes.error_message % 'invalid request',
                web2py_error="unknown application: '%s'" % self.application)

        #  set the application router
        #
        log_rewrite("select application=%s" % self.application)
        self.request.application = self.application
        if self.application not in routers:
            self.router = routers.BASE                # support gluon.main.wsgibase init->welcome
        else:
            self.router = routers[self.application]   # application router
        self.controllers = self.router.controllers
        self.default_controller = self.domain_controller or self.router.default_controller
        self.functions = self.router.functions
        self.languages = self.router.languages
        self.default_language = self.router.default_language
        self.map_hyphen = self.router.map_hyphen
        self.exclusive_domain = self.router.exclusive_domain
        self._acfe_match = self.router._acfe_match
        self.file_match = self.router.file_match
        self._file_match = self.router._file_match
        self._args_match = self.router._args_match

    def map_root_static(self):
        """
        Handles root-static files (no hyphen mapping)

        a root-static file is one whose incoming URL expects it to be at the root,
        typically robots.txt & favicon.ico
        """

        if len(self.args) == 1 and self.arg0 in self.router.root_static:
            self.controller = self.request.controller = 'static'
            root_static_file = pjoin(self.request.env.applications_parent,
                                     'applications', self.application,
                                     self.controller, self.arg0)
            log_rewrite("route: root static=%s" % root_static_file)
            return root_static_file, None
        return None, None

    def map_language(self):
        """Handles language (no hyphen mapping)"""
        arg0 = self.arg0  # no hyphen mapping
        if arg0 and self.languages and arg0 in self.languages:
            self.language = arg0
        else:
            self.language = self.default_language
        if self.language:
            log_rewrite("route: language=%s" % self.language)
            self.pop_arg_if(self.language == arg0)
            arg0 = self.arg0

    def map_controller(self):
        """Identifies controller"""
        #  handle controller
        #
        arg0 = self.harg0    # map hyphens
        if not arg0 or (self.controllers and arg0 not in self.controllers):
            self.controller = self.default_controller or ''
        else:
            self.controller = arg0
        self.pop_arg_if(arg0 == self.controller)
        log_rewrite("route: controller=%s" % self.controller)
        if not self.router._acfe_match.match(self.controller):
            raise HTTP(
                400, THREAD_LOCAL.routes.error_message % 'invalid request',
                web2py_error='invalid controller')

    def map_static(self):
        """
        Handles static files
        file_match but no hyphen mapping
        """
        if self.controller != 'static':
            return None, None
        version = regex_version.match(self.args(0))
        if self.args and version:
            file = '/'.join(self.args[1:])
        else:
            file = '/'.join(self.args)
        if len(self.args) == 0:
            bad_static = True   # require a file name
        elif '/' in self.file_match:
            # match the path
            bad_static = not self.router._file_match.match(file)
        else:
            # match path elements
            bad_static = False
            for name in self.args:
                bad_static = bad_static or name in (
                    '', '.', '..') or not self.router._file_match.match(name)
        if bad_static:
            log_rewrite('bad static path=%s' % file)
            raise HTTP(400,
                       THREAD_LOCAL.routes.error_message % 'invalid request',
                       web2py_error='invalid static file')
        #
        #  support language-specific static subdirectories,
        #  eg /appname/en/static/filename => applications/appname/static/en/filename
        #  if language-specific file doesn't exist, try same file in static
        #
        if self.language:
            static_file = pjoin(self.request.env.applications_parent,
                                'applications', self.application,
                                'static', self.language, file)
        if not self.language or not isfile(static_file):
            static_file = pjoin(self.request.env.applications_parent,
                                'applications', self.application,
                                'static', file)
        self.extension = None
        log_rewrite("route: static=%s" % static_file)
        return static_file, version

    def map_function(self):
        """Handles function.extension"""
        arg0 = self.harg0    # map hyphens
        functions = self.functions.get(self.controller, set())
        if isinstance(self.router.default_function, dict):
            default_function = self.router.default_function.get(
                self.controller, None)
        else:
            default_function = self.router.default_function  # str or None
        default_function = self.domain_function or default_function
        if not arg0 or functions and arg0 not in functions:
            self.function = default_function or ""
            self.pop_arg_if(arg0 and self.function == arg0)
        else:
            func_ext = arg0.split('.')
            if len(func_ext) > 1:
                self.function = func_ext[0]
                self.extension = func_ext[-1]
            else:
                self.function = arg0
            self.pop_arg_if(True)
        log_rewrite(
            "route: function.ext=%s.%s" % (self.function, self.extension))

        if not self.router._acfe_match.match(self.function):
            raise HTTP(
                400, THREAD_LOCAL.routes.error_message % 'invalid request',
                web2py_error='invalid function')
        if self.extension and not self.router._acfe_match.match(self.extension):
            raise HTTP(
                400, THREAD_LOCAL.routes.error_message % 'invalid request',
                web2py_error='invalid extension')

    def validate_args(self):
        """
        Checks args against validation pattern
        """
        for arg in self.args:
            if not self.router._args_match.match(arg):
                raise HTTP(
                    400, THREAD_LOCAL.routes.error_message % 'invalid request',
                    web2py_error='invalid arg <%s>' % arg)

    def sluggify(self):
        self.request.env.update(
            (k.lower().replace('.', '_'), v) for k, v in self.env.iteritems())

    def update_request(self):
        """
        Updates request from self
        Builds env.request_uri
        Makes lower-case versions of http headers in env
        """
        self.request.application = self.application
        self.request.controller = self.controller
        self.request.function = self.function
        self.request.extension = self.extension
        self.request.args = self.args
        if self.language:
            self.request.uri_language = self.language
        uri = '/%s/%s' % (self.controller, self.function)
        app = self.application
        if self.map_hyphen:
            uri = uri.replace('_', '-')
            app = app.replace('_', '-')
        if self.extension and self.extension != 'html':
            uri += '.' + self.extension
        if self.language:
            uri = '/%s%s' % (self.language, uri)
        uri = '/%s%s%s%s' % (
            app,
            uri,
            urllib.quote('/' + '/'.join(
                str(x) for x in self.args)) if self.args else '',
            ('?' + self.query) if self.query else '')
        self.env['REQUEST_URI'] = uri
        self.sluggify()

    @property
    def arg0(self):
        """Returns first arg"""
        return self.args(0)

    @property
    def harg0(self):
        """Returns first arg with optional hyphen mapping"""
        if self.map_hyphen and self.args(0):
            return self.args(0).replace('-', '_')
        return self.args(0)

    def pop_arg_if(self, dopop):
        """Conditionally removes first arg and returns new first arg"""
        if dopop:
            self.args.pop(0)


class MapUrlOut(object):
    """Logic for mapping outgoing URLs"""

    def __init__(self, request, env, application, controller,
                 function, args, other, scheme, host, port, language):
        """initialize a map-out object"""
        self.default_application = routers.BASE.default_application
        if application in routers:
            self.router = routers[application]
        else:
            self.router = routers.BASE
        self.request = request
        self.env = env
        self.application = application
        self.controller = controller
        self.is_static = (
            controller == 'static' or controller.startswith('static/'))
        self.function = function
        self.args = args
        self.other = other
        self.scheme = scheme
        self.host = host
        self.port = port
        self.language = language

        self.applications = routers.BASE.applications
        self.controllers = self.router.controllers
        self.functions = self.router.functions.get(self.controller, set())
        self.languages = self.router.languages
        self.default_language = self.router.default_language
        self.exclusive_domain = self.router.exclusive_domain
        self.map_hyphen = self.router.map_hyphen
        self.map_static = self.router.map_static
        self.path_prefix = routers.BASE.path_prefix

        self.domain_application = request and self.request.env.domain_application
        self.domain_controller = request and self.request.env.domain_controller
        if isinstance(self.router.default_function, dict):
            self.default_function = self.router.default_function.get(
                self.controller, None)
        else:
            self.default_function = self.router.default_function

        if (self.router.exclusive_domain
            and self.domain_application
            and self.domain_application != self.application
            and not self.host):
            raise SyntaxError('cross-domain conflict: must specify host')

        lang = self.language if self.language else request and request.uri_language
        if lang and self.languages and lang in self.languages:
            self.language = lang
        else:
            self.language = None

        self.omit_application = False
        self.omit_language = False
        self.omit_controller = False
        self.omit_function = False

    def omit_lang(self):
        """Omits language if possible"""
        if not self.language or self.language == self.default_language:
            self.omit_language = True

    def omit_acf(self):
        """Omits what we can of a/c/f"""
        router = self.router

        #  Handle the easy no-args case of tail-defaults: /a/c  /a  /
        #
        if not self.args and self.function == self.default_function:
            self.omit_function = True
            if self.controller == router.default_controller:
                self.omit_controller = True
                if self.application == self.default_application:
                    self.omit_application = True

        #  omit default application
        #  (which might be the domain default application)
        #
        default_application = self.domain_application or self.default_application
        if self.application == default_application:
            self.omit_application = True

        #  omit controller if default controller
        #
        default_controller = ((self.application == self.domain_application) and self.domain_controller) or router.default_controller or ''
        if self.controller == default_controller:
            self.omit_controller = True

        #  omit function if possible
        #
        if self.functions and self.function in self.functions and self.function == self.default_function:
            self.omit_function = True

        #  prohibit ambiguous cases
        #
        #  because we presume the lang string to be unambiguous, its presence protects application omission
        #
        if self.exclusive_domain:
            applications = [self.domain_application]
        else:
            applications = self.applications
        if self.omit_language:
            if not applications or self.controller in applications:
                self.omit_application = False
            if self.omit_application:
                if not applications or self.function in applications:
                    self.omit_controller = False
        if not self.controllers or self.function in self.controllers:
            self.omit_controller = False
        if self.args:
            if self.args[0] in self.functions or self.args[0] in self.controllers or self.args[0] in applications:
                self.omit_function = False
        if self.omit_controller:
            if self.function in self.controllers or self.function in applications:
                self.omit_controller = False
        if self.omit_application:
            if self.controller in applications:
                self.omit_application = False

        #  handle static as a special case
        #  (easier for external static handling)
        #
        if self.is_static:
            if not self.map_static:
                self.omit_application = False
                if self.language:
                    self.omit_language = False
            self.omit_controller = False
            self.omit_function = False

    def build_acf(self):
        "Builds a/c/f from components"
        acf = ''
        if self.map_hyphen:
            self.controller = self.controller.replace('_', '-')
            if self.controller != 'static' and not self.controller.startswith('static/'):
                self.application = self.application.replace('_', '-')
                self.function = self.function.replace('_', '-')
        if not self.omit_application:
            acf += '/' + self.application
        # handle case of flipping lang/static/file to static/lang/file for external rewrite
        if self.is_static and self.map_static is False and not self.omit_language:
            acf += '/' + self.controller + '/' + self.language
        else:
            if not self.omit_language:
                acf += '/' + self.language
            if not self.omit_controller:
                acf += '/' + self.controller
        if not self.omit_function:
            acf += '/' + self.function
        if self.path_prefix:
            acf = '/' + '/'.join(self.path_prefix) + acf
        if self.args:
            return acf
        return acf or '/'

    def acf(self):
        """Converts components to /app/lang/controller/function"""
        if not routers:
            return None         # use regex filter
        self.omit_lang()        # try to omit language
        self.omit_acf()         # try to omit a/c/f
        return self.build_acf()  # build and return the /a/lang/c/f string


def map_url_in(request, env, app=False):
    """Routes incoming URL"""
    #  initialize router-url object
    #
    THREAD_LOCAL.routes = params  # default to base routes
    map = MapUrlIn(request=request, env=env)
    map.sluggify()
    map.map_prefix()  # strip prefix if present
    map.map_app()     # determine application

    #  configure THREAD_LOCAL.routes for error rewrite
    #
    if params.routes_app:
        THREAD_LOCAL.routes = params_apps.get(app, params)

    if app:
        return map.application

    root_static_file, version = map.map_root_static(
    )  # handle root-static files
    if root_static_file:
        map.update_request()
        return (root_static_file, version, map.env)
    # handle mapping of lang/static to static/lang in externally-rewritten URLs
    # in case we have to handle them ourselves
    if map.languages and map.map_static is False and map.arg0 == 'static' and map.args(1) in map.languages:
        map.map_controller()
        map.map_language()
    else:
        map.map_language()
        map.map_controller()
    static_file, version = map.map_static()
    if static_file:
        map.update_request()
        return (static_file, version, map.env)
    map.map_function()
    map.validate_args()
    map.update_request()
    return (None, None, map.env)


def map_url_out(request, env, application, controller,
                function, args, other, scheme, host, port, language=None):
    """
    Supply /a/c/f (or /a/lang/c/f) portion of outgoing url

    The basic rule is that we can only make transformations
    that map_url_in can reverse.

    Suppose that the incoming arguments are a,c,f,args,lang
    and that the router defaults are da, dc, df, dl.

    We can perform these transformations trivially if args=[] and lang=None or dl::

        /da/dc/df => /
        /a/dc/df => /a
        /a/c/df => /a/c

    We would also like to be able to strip the default application or application/controller
    from URLs with function/args present, thus::

        /da/c/f/args  => /c/f/args
        /da/dc/f/args => /f/args

    We use [applications] and [controllers] and {functions} to suppress ambiguous omissions.

    We assume that language names do not collide with a/c/f names.
    """
    map = MapUrlOut(request, env, application, controller,
                    function, args, other, scheme, host, port, language)
    return map.acf()


def get_effective_router(appname):
    """Returns a private copy of the effective router for the specified application"""
    if not routers or appname not in routers:
        return None
    return Storage(routers[appname])  # return a copy
