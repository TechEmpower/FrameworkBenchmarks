#!/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

The gluon wsgi application
---------------------------
"""

if False: import import_all # DO NOT REMOVE PART OF FREEZE PROCESS
import gc
import Cookie
import os
import re
import copy
import sys
import time
import datetime
import signal
import socket
import random
import urllib2
import string


try:
    import simplejson as sj #external installed library
except:
    try:
        import json as sj #standard installed library
    except:
        import gluon.contrib.simplejson as sj #pure python library

from thread import allocate_lock

from gluon.fileutils import abspath, write_file
from gluon.settings import global_settings
from gluon.utils import web2py_uuid
from gluon.admin import add_path_first, create_missing_folders, create_missing_app_folders
from gluon.globals import current

#  Remarks:
#  calling script has inserted path to script directory into sys.path
#  applications_parent (path to applications/, site-packages/ etc)
#  defaults to that directory set sys.path to
#  ("", gluon_parent/site-packages, gluon_parent, ...)
#
#  this is wrong:
#  web2py_path = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#  because we do not want the path to this file which may be Library.zip
#  gluon_parent is the directory containing gluon, web2py.py, logging.conf
#  and the handlers.
#  applications_parent (web2py_path) is the directory containing applications/
#  and routes.py
#  The two are identical unless web2py_path is changed via the web2py.py -f folder option
#  main.web2py_path is the same as applications_parent (for backward compatibility)

web2py_path = global_settings.applications_parent  # backward compatibility

create_missing_folders()

# set up logging for subsequent imports
import logging
import logging.config

# This needed to prevent exception on Python 2.5:
# NameError: name 'gluon' is not defined
# See http://bugs.python.org/issue1436

# attention!, the import Tkinter in messageboxhandler, changes locale ...
import gluon.messageboxhandler
logging.gluon = gluon
# so we must restore it! Thanks ozancag
import locale
locale.setlocale(locale.LC_CTYPE, "C") # IMPORTANT, web2py requires locale "C"

exists = os.path.exists
pjoin = os.path.join

logpath = abspath("logging.conf")
if exists(logpath):
    logging.config.fileConfig(abspath("logging.conf"))
else:
    logging.basicConfig()
logger = logging.getLogger("web2py")

from gluon.restricted import RestrictedError
from gluon.http import HTTP, redirect
from gluon.globals import Request, Response, Session
from gluon.compileapp import build_environment, run_models_in, \
    run_controller_in, run_view_in
from gluon.contenttype import contenttype
from pydal.base import BaseAdapter
from gluon.validators import CRYPT
from gluon.html import URL, xmlescape
from gluon.utils import is_valid_ip_address, getipaddrinfo
from gluon.rewrite import load as load_routes, url_in, THREAD_LOCAL as rwthread, \
    try_rewrite_on_error, fixup_missing_path_info
from gluon import newcron

__all__ = ['wsgibase', 'save_password', 'appfactory', 'HttpServer']

requests = 0    # gc timer

# Security Checks: validate URL and session_id here,
# accept_language is validated in languages

# pattern used to validate client address
regex_client = re.compile('[\w\-:]+(\.[\w\-]+)*\.?')  # ## to account for IPV6

try:
    version_info = open(pjoin(global_settings.gluon_parent, 'VERSION'), 'r')
    raw_version_string = version_info.read().split()[-1].strip()
    version_info.close()
    global_settings.web2py_version = raw_version_string
    web2py_version = global_settings.web2py_version
except:
    raise RuntimeError("Cannot determine web2py version")

try:
    from gluon import rocket
except:
    if not global_settings.web2py_runtime_gae:
        logger.warn('unable to import Rocket')

load_routes()

HTTPS_SCHEMES = set(('https', 'HTTPS'))


def get_client(env):
    """
    Guesses the client address from the environment variables

    First tries 'http_x_forwarded_for', secondly 'remote_addr'
    if all fails, assume '127.0.0.1' or '::1' (running locally)
    """
    eget = env.get
    g = regex_client.search(eget('http_x_forwarded_for', ''))
    client = (g.group() or '').split(',')[0] if g else None
    if client in (None, '', 'unknown'):
        g = regex_client.search(eget('remote_addr', ''))
        if g:
            client = g.group()
        elif env.http_host.startswith('['):  # IPv6
            client = '::1'
        else:
            client = '127.0.0.1'  # IPv4
    if not is_valid_ip_address(client):
        raise HTTP(400, "Bad Request (request.client=%s)" % client)
    return client


def serve_controller(request, response, session):
    """
    This function is used to generate a dynamic page.
    It first runs all models, then runs the function in the controller,
    and then tries to render the output using a view/template.
    this function must run from the [application] folder.
    A typical example would be the call to the url
    /[application]/[controller]/[function] that would result in a call
    to [function]() in applications/[application]/[controller].py
    rendered by applications/[application]/views/[controller]/[function].html
    """

    # ##################################################
    # build environment for controller and view
    # ##################################################

    environment = build_environment(request, response, session)

    # set default view, controller can override it

    response.view = '%s/%s.%s' % (request.controller,
                                  request.function,
                                  request.extension)

    # also, make sure the flash is passed through
    # ##################################################
    # process models, controller and view (if required)
    # ##################################################

    run_models_in(environment)
    response._view_environment = copy.copy(environment)
    page = run_controller_in(request.controller, request.function, environment)
    if isinstance(page, dict):
        response._vars = page
        response._view_environment.update(page)
        run_view_in(response._view_environment)
        page = response.body.getvalue()
    # logic to garbage collect after exec, not always, once every 100 requests
    global requests
    requests = ('requests' in globals()) and (requests + 1) % 100 or 0
    if not requests:
        gc.collect()
    # end garbage collection logic

    # ##################################################
    # set default headers it not set
    # ##################################################

    default_headers = [
        ('Content-Type', contenttype('.' + request.extension)),
        ('Cache-Control',
         'no-store, no-cache, must-revalidate, post-check=0, pre-check=0'),
        ('Expires', time.strftime('%a, %d %b %Y %H:%M:%S GMT',
                                  time.gmtime())),
        ('Pragma', 'no-cache')]
    for key, value in default_headers:
        response.headers.setdefault(key, value)

    raise HTTP(response.status, page, **response.headers)


class LazyWSGI(object):
    def __init__(self, environ, request, response):
        self.wsgi_environ = environ
        self.request = request
        self.response = response

    @property
    def environ(self):
        if not hasattr(self, '_environ'):
            new_environ = self.wsgi_environ
            new_environ['wsgi.input'] = self.request.body
            new_environ['wsgi.version'] = 1
            self._environ = new_environ
        return self._environ

    def start_response(self, status='200', headers=[], exec_info=None):
        """
        in controller you can use:

        - request.wsgi.environ
        - request.wsgi.start_response

        to call third party WSGI applications
        """
        self.response.status = str(status).split(' ', 1)[0]
        self.response.headers = dict(headers)
        return lambda *args, **kargs: \
            self.response.write(escape=False, *args, **kargs)

    def middleware(self, *middleware_apps):
        """
        In you controller use::

            @request.wsgi.middleware(middleware1, middleware2, ...)

        to decorate actions with WSGI middleware. actions must return strings.
        uses a simulated environment so it may have weird behavior in some cases
        """
        def middleware(f):
            def app(environ, start_response):
                data = f()
                start_response(self.response.status,
                               self.response.headers.items())
                if isinstance(data, list):
                    return data
                return [data]
            for item in middleware_apps:
                app = item(app)
            def caller(app):
                return app(self.environ, self.start_response)
            return lambda caller=caller, app=app: caller(app)
        return middleware


def wsgibase(environ, responder):
    """
    The gluon wsgi application. The first function called when a page
    is requested (static or dynamic). It can be called by paste.httpserver
    or by apache mod_wsgi (or any WSGI-compatible server).

      - fills request with info
      - the environment variables, replacing '.' with '_'
      - adds web2py path and version info
      - compensates for fcgi missing path_info and query_string
      - validates the path in url

    The url path must be either:

    1. for static pages:

      - /<application>/static/<file>

    2. for dynamic pages:

      - /<application>[/<controller>[/<function>[/<sub>]]][.<extension>]

    The naming conventions are:

      - application, controller, function and extension may only contain
        `[a-zA-Z0-9_]`
      - file and sub may also contain '-', '=', '.' and '/'
    """
    eget = environ.get
    current.__dict__.clear()
    request = Request(environ)
    response = Response()
    session = Session()
    env = request.env
    #env.web2py_path = global_settings.applications_parent
    env.web2py_version = web2py_version
    #env.update(global_settings)
    static_file = False
    try:
        try:
            try:
                # ##################################################
                # handle fcgi missing path_info and query_string
                # select rewrite parameters
                # rewrite incoming URL
                # parse rewritten header variables
                # parse rewritten URL
                # serve file if static
                # ##################################################

                fixup_missing_path_info(environ)
                (static_file, version, environ) = url_in(request, environ)
                response.status = env.web2py_status_code or response.status

                if static_file:
                    if eget('QUERY_STRING', '').startswith('attachment'):
                        response.headers['Content-Disposition'] \
                            = 'attachment'
                    if version:
                        response.headers['Cache-Control'] = 'max-age=315360000'
                        response.headers[
                            'Expires'] = 'Thu, 31 Dec 2037 23:59:59 GMT'
                    response.stream(static_file, request=request)


                # ##################################################
                # fill in request items
                # ##################################################
                app = request.application  # must go after url_in!

                if not global_settings.local_hosts:
                    local_hosts = set(['127.0.0.1', '::ffff:127.0.0.1', '::1'])
                    if not global_settings.web2py_runtime_gae:
                        try:
                            fqdn = socket.getfqdn()
                            local_hosts.add(socket.gethostname())
                            local_hosts.add(fqdn)
                            local_hosts.update([
                                addrinfo[4][0] for addrinfo
                                in getipaddrinfo(fqdn)])
                            if env.server_name:
                                local_hosts.add(env.server_name)
                                local_hosts.update([
                                        addrinfo[4][0] for addrinfo
                                        in getipaddrinfo(env.server_name)])
                        except (socket.gaierror, TypeError):
                            pass
                    global_settings.local_hosts = list(local_hosts)
                else:
                    local_hosts = global_settings.local_hosts
                client = get_client(env)
                x_req_with = str(env.http_x_requested_with).lower()
                cmd_opts = request.global_settings.cmd_options

                request.update(
                    client = client,
                    folder = abspath('applications', app) + os.sep,
                    ajax = x_req_with == 'xmlhttprequest',
                    cid = env.http_web2py_component_element,
                    is_local = (env.remote_addr in local_hosts and
                                client == env.remote_addr),
                    is_shell = cmd_opts and cmd_opts.shell,
                    is_sheduler = cmd_opts and cmd_opts.scheduler,
                    is_https = env.wsgi_url_scheme in HTTPS_SCHEMES or \
                        request.env.http_x_forwarded_proto in HTTPS_SCHEMES \
                        or env.https == 'on'
                    )
                request.compute_uuid()  # requires client
                request.url = environ['PATH_INFO']

                # ##################################################
                # access the requested application
                # ##################################################

                disabled = pjoin(request.folder, 'DISABLED')
                if not exists(request.folder):
                    if app == rwthread.routes.default_application \
                            and app != 'welcome':
                        redirect(URL('welcome', 'default', 'index'))
                    elif rwthread.routes.error_handler:
                        _handler = rwthread.routes.error_handler
                        redirect(URL(_handler['application'],
                                     _handler['controller'],
                                     _handler['function'],
                                     args=app))
                    else:
                        raise HTTP(404, rwthread.routes.error_message
                                   % 'invalid request',
                                   web2py_error='invalid application')
                elif not request.is_local and exists(disabled):
                    raise HTTP(503, "<html><body><h1>Temporarily down for maintenance</h1></body></html>")

                # ##################################################
                # build missing folders
                # ##################################################

                create_missing_app_folders(request)

                # ##################################################
                # get the GET and POST data
                # ##################################################

                #parse_get_post_vars(request, environ)

                # ##################################################
                # expose wsgi hooks for convenience
                # ##################################################

                request.wsgi = LazyWSGI(environ, request, response)

                # ##################################################
                # load cookies
                # ##################################################

                if env.http_cookie:
                    try:
                        request.cookies.load(env.http_cookie)
                    except Cookie.CookieError, e:
                        pass  # invalid cookies

                # ##################################################
                # try load session or create new session file
                # ##################################################

                if not env.web2py_disable_session:
                    session.connect(request, response)

                # ##################################################
                # run controller
                # ##################################################

                if global_settings.debugging and app != "admin":
                    import gluon.debug
                    # activate the debugger
                    gluon.debug.dbg.do_debug(mainpyfile=request.folder)

                serve_controller(request, response, session)

            except HTTP, http_response:

                if static_file:
                    return http_response.to(responder, env=env)

                if request.body:
                    request.body.close()

                if hasattr(current, 'request'):

                    # ##################################################
                    # on success, try store session in database
                    # ##################################################
                    session._try_store_in_db(request, response)

                    # ##################################################
                    # on success, commit database
                    # ##################################################

                    if response.do_not_commit is True:
                        BaseAdapter.close_all_instances(None)
                    elif response.custom_commit:
                        BaseAdapter.close_all_instances(response.custom_commit)
                    else:
                        BaseAdapter.close_all_instances('commit')

                    # ##################################################
                    # if session not in db try store session on filesystem
                    # this must be done after trying to commit database!
                    # ##################################################

                    session._try_store_in_cookie_or_file(request, response)

                    # Set header so client can distinguish component requests.
                    if request.cid:
                        http_response.headers.setdefault(
                            'web2py-component-content', 'replace')

                    if request.ajax:
                        if response.flash:
                            http_response.headers['web2py-component-flash'] = \
                                urllib2.quote(xmlescape(response.flash).replace('\n', ''))
                        if response.js:
                            http_response.headers['web2py-component-command'] = \
                                urllib2.quote(response.js.replace('\n', ''))

                    # ##################################################
                    # store cookies in headers
                    # ##################################################

                    session._fixup_before_save()
                    http_response.cookies2headers(response.cookies)

                ticket = None

            except RestrictedError, e:

                if request.body:
                    request.body.close()

                # ##################################################
                # on application error, rollback database
                # ##################################################

                # log tickets before rollback if not in DB
                if not request.tickets_db:
                    ticket = e.log(request) or 'unknown'
                # rollback
                if response._custom_rollback:
                    response._custom_rollback()
                else:
                    BaseAdapter.close_all_instances('rollback')
                # if tickets in db, reconnect and store it in db
                if request.tickets_db:
                    ticket = e.log(request) or 'unknown'

                http_response = \
                    HTTP(500, rwthread.routes.error_message_ticket %
                         dict(ticket=ticket),
                         web2py_error='ticket %s' % ticket)

        except:

            if request.body:
                request.body.close()

            # ##################################################
            # on application error, rollback database
            # ##################################################

            try:
                if response._custom_rollback:
                    response._custom_rollback()
                else:
                    BaseAdapter.close_all_instances('rollback')
            except:
                pass
            e = RestrictedError('Framework', '', '', locals())
            ticket = e.log(request) or 'unrecoverable'
            http_response = \
                HTTP(500, rwthread.routes.error_message_ticket
                     % dict(ticket=ticket),
                     web2py_error='ticket %s' % ticket)

    finally:
        if response and hasattr(response, 'session_file') \
                and response.session_file:
            response.session_file.close()

    session._unlock(response)
    http_response, new_environ = try_rewrite_on_error(
        http_response, request, environ, ticket)
    if not http_response:
        return wsgibase(new_environ, responder)
    if global_settings.web2py_crontype == 'soft':
        newcron.softcron(global_settings.applications_parent).start()
    return http_response.to(responder, env=env)


def save_password(password, port):
    """
    Used by main() to save the password in the parameters_port.py file.
    """

    password_file = abspath('parameters_%i.py' % port)
    if password == '<random>':
        # make up a new password
        chars = string.letters + string.digits
        password = ''.join([random.choice(chars) for _ in range(8)])
        cpassword = CRYPT()(password)[0]
        print '******************* IMPORTANT!!! ************************'
        print 'your admin password is "%s"' % password
        print '*********************************************************'
    elif password == '<recycle>':
        # reuse the current password if any
        if exists(password_file):
            return
        else:
            password = ''
    elif password.startswith('<pam_user:'):
        # use the pam password for specified user
        cpassword = password[1:-1]
    else:
        # use provided password
        cpassword = CRYPT()(password)[0]
    fp = open(password_file, 'w')
    if password:
        fp.write('password="%s"\n' % cpassword)
    else:
        fp.write('password=None\n')
    fp.close()


def appfactory(wsgiapp=wsgibase,
               logfilename='httpserver.log',
               profiler_dir=None,
               profilerfilename=None):
    """
    generates a wsgi application that does logging and profiling and calls
    wsgibase

    Args:
        wsgiapp: the base application
        logfilename: where to store apache-compatible requests log
        profiler_dir: where to store profile files

    """
    if profilerfilename is not None:
        raise BaseException("Deprecated API")
    if profiler_dir:
        profiler_dir = abspath(profiler_dir)
        logger.warn('profiler is on. will use dir %s', profiler_dir)
        if not os.path.isdir(profiler_dir):
            try:
                os.makedirs(profiler_dir)
            except:
                raise BaseException("Can't create dir %s" % profiler_dir)
        filepath = pjoin(profiler_dir, 'wtest')
        try:
            filehandle = open( filepath, 'w' )
            filehandle.close()
            os.unlink(filepath)
        except IOError:
            raise BaseException("Unable to write to dir %s" % profiler_dir)

    def app_with_logging(environ, responder):
        """
        a wsgi app that does logging and profiling and calls wsgibase
        """
        status_headers = []

        def responder2(s, h):
            """
            wsgi responder app
            """
            status_headers.append(s)
            status_headers.append(h)
            return responder(s, h)

        time_in = time.time()
        ret = [0]
        if not profiler_dir:
            ret[0] = wsgiapp(environ, responder2)
        else:
            import cProfile
            prof = cProfile.Profile()
            prof.enable()
            ret[0] = wsgiapp(environ, responder2)
            prof.disable()
            destfile = pjoin(profiler_dir, "req_%s.prof" % web2py_uuid())
            prof.dump_stats(destfile)

        try:
            line = '%s, %s, %s, %s, %s, %s, %f\n' % (
                environ['REMOTE_ADDR'],
                datetime.datetime.today().strftime('%Y-%m-%d %H:%M:%S'),
                environ['REQUEST_METHOD'],
                environ['PATH_INFO'].replace(',', '%2C'),
                environ['SERVER_PROTOCOL'],
                (status_headers[0])[:3],
                time.time() - time_in,
            )
            if not logfilename:
                sys.stdout.write(line)
            elif isinstance(logfilename, str):
                write_file(logfilename, line, 'a')
            else:
                logfilename.write(line)
        except:
            pass
        return ret[0]

    return app_with_logging


class HttpServer(object):
    """
    the web2py web server (Rocket)
    """

    def __init__(
        self,
        ip='127.0.0.1',
        port=8000,
        password='',
        pid_filename='httpserver.pid',
        log_filename='httpserver.log',
        profiler_dir=None,
        ssl_certificate=None,
        ssl_private_key=None,
        ssl_ca_certificate=None,
        min_threads=None,
        max_threads=None,
        server_name=None,
        request_queue_size=5,
        timeout=10,
        socket_timeout=1,
        shutdown_timeout=None,  # Rocket does not use a shutdown timeout
        path=None,
        interfaces=None  # Rocket is able to use several interfaces - must be list of socket-tuples as string
    ):
        """
        starts the web server.
        """

        if interfaces:
            # if interfaces is specified, it must be tested for rocket parameter correctness
            # not necessarily completely tested (e.g. content of tuples or ip-format)
            import types
            if isinstance(interfaces, types.ListType):
                for i in interfaces:
                    if not isinstance(i, types.TupleType):
                        raise "Wrong format for rocket interfaces parameter - see http://packages.python.org/rocket/"
            else:
                raise "Wrong format for rocket interfaces parameter - see http://packages.python.org/rocket/"

        if path:
            # if a path is specified change the global variables so that web2py
            # runs from there instead of cwd or os.environ['web2py_path']
            global web2py_path
            path = os.path.normpath(path)
            web2py_path = path
            global_settings.applications_parent = path
            os.chdir(path)
            load_routes()
            for p in (path, abspath('site-packages'), ""):
                add_path_first(p)
            if exists("logging.conf"):
                logging.config.fileConfig("logging.conf")

        save_password(password, port)
        self.pid_filename = pid_filename
        if not server_name:
            server_name = socket.gethostname()
        logger.info('starting web server...')
        rocket.SERVER_NAME = server_name
        rocket.SOCKET_TIMEOUT = socket_timeout
        sock_list = [ip, port]
        if not ssl_certificate or not ssl_private_key:
            logger.info('SSL is off')
        elif not rocket.ssl:
            logger.warning('Python "ssl" module unavailable. SSL is OFF')
        elif not exists(ssl_certificate):
            logger.warning('unable to open SSL certificate. SSL is OFF')
        elif not exists(ssl_private_key):
            logger.warning('unable to open SSL private key. SSL is OFF')
        else:
            sock_list.extend([ssl_private_key, ssl_certificate])
            if ssl_ca_certificate:
                sock_list.append(ssl_ca_certificate)

            logger.info('SSL is ON')
        app_info = {'wsgi_app': appfactory(wsgibase,
                                           log_filename,
                                           profiler_dir)}

        self.server = rocket.Rocket(interfaces or tuple(sock_list),
                                    method='wsgi',
                                    app_info=app_info,
                                    min_threads=min_threads,
                                    max_threads=max_threads,
                                    queue_size=int(request_queue_size),
                                    timeout=int(timeout),
                                    handle_signals=False,
                                    )

    def start(self):
        """
        start the web server
        """
        try:
            signal.signal(signal.SIGTERM, lambda a, b, s=self: s.stop())
            signal.signal(signal.SIGINT, lambda a, b, s=self: s.stop())
        except:
            pass
        write_file(self.pid_filename, str(os.getpid()))
        self.server.start()

    def stop(self, stoplogging=False):
        """
        stop cron and the web server
        """
        newcron.stopcron()
        self.server.stop(stoplogging)
        try:
            os.unlink(self.pid_filename)
        except:
            pass
