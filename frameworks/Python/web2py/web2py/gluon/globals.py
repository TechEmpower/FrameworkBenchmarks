#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

Contains the classes for the global used variables:

- Request
- Response
- Session

"""

from gluon.storage import Storage, List
from gluon.streamer import streamer, stream_file_or_304_or_206, DEFAULT_CHUNK_SIZE
from gluon.xmlrpc import handler
from gluon.contenttype import contenttype
from gluon.html import xmlescape, TABLE, TR, PRE, URL
from gluon.http import HTTP, redirect
from gluon.fileutils import up
from gluon.serializers import json, custom_json
import gluon.settings as settings
from gluon.utils import web2py_uuid, secure_dumps, secure_loads
from gluon.settings import global_settings
from gluon import recfile
import hashlib
import portalocker
try:
    import cPickle as pickle
except:
    import pickle
from pickle import Pickler, MARK, DICT, EMPTY_DICT
from types import DictionaryType
import cStringIO
import datetime
import re
import copy_reg
import Cookie
import os
import sys
import traceback
import threading
import cgi
import urlparse
import copy
import tempfile
from gluon.cache import CacheInRam
from gluon.fileutils import copystream

FMT = '%a, %d-%b-%Y %H:%M:%S PST'
PAST = 'Sat, 1-Jan-1971 00:00:00'
FUTURE = 'Tue, 1-Dec-2999 23:59:59'

try:
    from gluon.contrib.minify import minify
    have_minify = True
except ImportError:
    have_minify = False

try:
    import simplejson as sj  # external installed library
except:
    try:
        import json as sj  # standard installed library
    except:
        import gluon.contrib.simplejson as sj  # pure python library

regex_session_id = re.compile('^([\w\-]+/)?[\w\-\.]+$')

__all__ = ['Request', 'Response', 'Session']

current = threading.local()  # thread-local storage for request-scope globals

css_template = '<link href="%s" rel="stylesheet" type="text/css" />'
js_template = '<script src="%s" type="text/javascript"></script>'
coffee_template = '<script src="%s" type="text/coffee"></script>'
typescript_template = '<script src="%s" type="text/typescript"></script>'
less_template = '<link href="%s" rel="stylesheet/less" type="text/css" />'
css_inline = '<style type="text/css">\n%s\n</style>'
js_inline = '<script type="text/javascript">\n%s\n</script>'


# IMPORTANT:
# this is required so that pickled dict(s) and class.__dict__
# are sorted and web2py can detect without ambiguity when a session changes
class SortingPickler(Pickler):
    def save_dict(self, obj):
        self.write(EMPTY_DICT if self.bin else MARK+DICT)
        self.memoize(obj)
        self._batch_setitems([(key, obj[key]) for key in sorted(obj)])

SortingPickler.dispatch = copy.copy(Pickler.dispatch)
SortingPickler.dispatch[DictionaryType] = SortingPickler.save_dict


def sorting_dumps(obj, protocol=None):
    file = cStringIO.StringIO()
    SortingPickler(file, protocol).dump(obj)
    return file.getvalue()
# END #####################################################################


def copystream_progress(request, chunk_size=10 ** 5):
    """
    Copies request.env.wsgi_input into request.body
    and stores progress upload status in cache_ram
    X-Progress-ID:length and X-Progress-ID:uploaded
    """
    env = request.env
    if not env.get('CONTENT_LENGTH', None):
        return cStringIO.StringIO()
    source = env['wsgi.input']
    try:
        size = int(env['CONTENT_LENGTH'])
    except ValueError:
        raise HTTP(400, "Invalid Content-Length header")
    try:  # Android requires this
        dest = tempfile.NamedTemporaryFile()
    except NotImplementedError:  # and GAE this
        dest = tempfile.TemporaryFile()
    if not 'X-Progress-ID' in request.get_vars:
        copystream(source, dest, size, chunk_size)
        return dest
    cache_key = 'X-Progress-ID:' + request.get_vars['X-Progress-ID']
    cache_ram = CacheInRam(request)  # same as cache.ram because meta_storage
    cache_ram(cache_key + ':length', lambda: size, 0)
    cache_ram(cache_key + ':uploaded', lambda: 0, 0)
    while size > 0:
        if size < chunk_size:
            data = source.read(size)
            cache_ram.increment(cache_key + ':uploaded', size)
        else:
            data = source.read(chunk_size)
            cache_ram.increment(cache_key + ':uploaded', chunk_size)
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
    cache_ram(cache_key + ':length', None)
    cache_ram(cache_key + ':uploaded', None)
    return dest


class Request(Storage):

    """
    Defines the request object and the default values of its members

    - env: environment variables, by gluon.main.wsgibase()
    - cookies
    - get_vars
    - post_vars
    - vars
    - folder
    - application
    - function
    - args
    - extension
    - now: datetime.datetime.now()
    - utcnow : datetime.datetime.utcnow()
    - is_local
    - is_https
    - restful()
    """

    def __init__(self, env):
        Storage.__init__(self)
        self.env = Storage(env)
        self.env.web2py_path = global_settings.applications_parent
        self.env.update(global_settings)
        self.cookies = Cookie.SimpleCookie()
        self._get_vars = None
        self._post_vars = None
        self._vars = None
        self._body = None
        self.folder = None
        self.application = None
        self.function = None
        self.args = List()
        self.extension = 'html'
        self.now = datetime.datetime.now()
        self.utcnow = datetime.datetime.utcnow()
        self.is_restful = False
        self.is_https = False
        self.is_local = False
        self.global_settings = settings.global_settings

    def parse_get_vars(self):
        """Takes the QUERY_STRING and unpacks it to get_vars
        """
        query_string = self.env.get('QUERY_STRING', '')
        dget = urlparse.parse_qs(query_string, keep_blank_values=1)  # Ref: https://docs.python.org/2/library/cgi.html#cgi.parse_qs
        get_vars = self._get_vars = Storage(dget)
        for (key, value) in get_vars.iteritems():
            if isinstance(value, list) and len(value) == 1:
                get_vars[key] = value[0]

    def parse_post_vars(self):
        """Takes the body of the request and unpacks it into
        post_vars. application/json is also automatically parsed
        """
        env = self.env
        post_vars = self._post_vars = Storage()
        body = self.body
        # if content-type is application/json, we must read the body
        is_json = env.get('content_type', '')[:16] == 'application/json'

        if is_json:
            try:
                json_vars = sj.load(body)
            except:
                # incoherent request bodies can still be parsed "ad-hoc"
                json_vars = {}
                pass
            # update vars and get_vars with what was posted as json
            if isinstance(json_vars, dict):
                post_vars.update(json_vars)

            body.seek(0)

        # parse POST variables on POST, PUT, BOTH only in post_vars
        if (body and not is_json
            and env.request_method in ('POST', 'PUT', 'DELETE', 'BOTH')):
            query_string = env.pop('QUERY_STRING', None)
            dpost = cgi.FieldStorage(fp=body, environ=env, keep_blank_values=1)
            try:
                post_vars.update(dpost)
            except:
                pass
            if query_string is not None:
                env['QUERY_STRING'] = query_string
            # The same detection used by FieldStorage to detect multipart POSTs
            body.seek(0)

            def listify(a):
                return (not isinstance(a, list) and [a]) or a
            try:
                keys = sorted(dpost)
            except TypeError:
                keys = []
            for key in keys:
                if key is None:
                    continue  # not sure why cgi.FieldStorage returns None key
                dpk = dpost[key]
                # if an element is not a file replace it with
                # its value else leave it alone

                pvalue = listify([(_dpk if _dpk.filename else _dpk.value)
                                  for _dpk in dpk]
                                 if isinstance(dpk, list) else
                                 (dpk if dpk.filename else dpk.value))
                if len(pvalue):
                    post_vars[key] = (len(pvalue) > 1 and pvalue) or pvalue[0]

    @property
    def body(self):
        if self._body is None:
            try:
                self._body = copystream_progress(self)
            except IOError:
                raise HTTP(400, "Bad Request - HTTP body is incomplete")
        return self._body

    def parse_all_vars(self):
        """Merges get_vars and post_vars to vars
        """
        self._vars = copy.copy(self.get_vars)
        for key, value in self.post_vars.iteritems():
            if not key in self._vars:
                self._vars[key] = value
            else:
                if not isinstance(self._vars[key], list):
                    self._vars[key] = [self._vars[key]]
                self._vars[key] += value if isinstance(value, list) else [value]

    @property
    def get_vars(self):
        """Lazily parses the query string into get_vars
        """
        if self._get_vars is None:
            self.parse_get_vars()
        return self._get_vars

    @property
    def post_vars(self):
        """Lazily parse the body into post_vars
        """
        if self._post_vars is None:
            self.parse_post_vars()
        return self._post_vars

    @property
    def vars(self):
        """Lazily parses all get_vars and post_vars to fill vars
        """
        if self._vars is None:
            self.parse_all_vars()
        return self._vars

    def compute_uuid(self):
        self.uuid = '%s/%s.%s.%s' % (
            self.application,
            self.client.replace(':', '_'),
            self.now.strftime('%Y-%m-%d.%H-%M-%S'),
            web2py_uuid())
        return self.uuid

    def user_agent(self):
        from gluon.contrib import user_agent_parser
        session = current.session
        user_agent = session._user_agent
        if user_agent:
            return user_agent
        user_agent = user_agent_parser.detect(self.env.http_user_agent)
        for key, value in user_agent.items():
            if isinstance(value, dict):
                user_agent[key] = Storage(value)
        user_agent = session._user_agent = Storage(user_agent)
        return user_agent

    def requires_https(self):
        """
        If request comes in over HTTP, redirects it to HTTPS
        and secures the session.
        """
        cmd_opts = global_settings.cmd_options
        # checking if this is called within the scheduler or within the shell
        # in addition to checking if it's not a cronjob
        if ((cmd_opts and (cmd_opts.shell or cmd_opts.scheduler))
                or global_settings.cronjob or self.is_https):
            current.session.secure()
        else:
            current.session.forget()
            redirect(URL(scheme='https', args=self.args, vars=self.vars))

    def restful(self):
        def wrapper(action, self=self):
            def f(_action=action, _self=self, *a, **b):
                self.is_restful = True
                method = _self.env.request_method
                if len(_self.args) and '.' in _self.args[-1]:
                    _self.args[-1], _, self.extension = self.args[-1].rpartition('.')
                    current.response.headers['Content-Type'] = \
                        contenttype('.' + _self.extension.lower())
                rest_action = _action().get(method, None)
                if not (rest_action and method == method.upper()
                        and callable(rest_action)):
                    raise HTTP(405, "method not allowed")
                try:
                    return rest_action(*_self.args, **getattr(_self, 'vars', {}))
                except TypeError, e:
                    exc_type, exc_value, exc_traceback = sys.exc_info()
                    if len(traceback.extract_tb(exc_traceback)) == 1:
                        raise HTTP(400, "invalid arguments")
                    else:
                        raise
            f.__doc__ = action.__doc__
            f.__name__ = action.__name__
            return f
        return wrapper


class Response(Storage):

    """
    Defines the response object and the default values of its members
    response.write(   ) can be used to write in the output html
    """

    def __init__(self):
        Storage.__init__(self)
        self.status = 200
        self.headers = dict()
        self.headers['X-Powered-By'] = 'web2py'
        self.body = cStringIO.StringIO()
        self.session_id = None
        self.cookies = Cookie.SimpleCookie()
        self.postprocessing = []
        self.flash = ''            # used by the default view layout
        self.meta = Storage()      # used by web2py_ajax.html
        self.menu = []             # used by the default view layout
        self.files = []            # used by web2py_ajax.html
        self._vars = None
        self._caller = lambda f: f()
        self._view_environment = None
        self._custom_commit = None
        self._custom_rollback = None
        self.generic_patterns = ['*']
        self.delimiters = ('{{', '}}')
        self.formstyle = 'table3cols'
        self.form_label_separator = ': '

    def write(self, data, escape=True):
        if not escape:
            self.body.write(str(data))
        else:
            self.body.write(xmlescape(data))

    def render(self, *a, **b):
        from compileapp import run_view_in
        if len(a) > 2:
            raise SyntaxError(
                'Response.render can be called with two arguments, at most')
        elif len(a) == 2:
            (view, self._vars) = (a[0], a[1])
        elif len(a) == 1 and isinstance(a[0], str):
            (view, self._vars) = (a[0], {})
        elif len(a) == 1 and hasattr(a[0], 'read') and callable(a[0].read):
            (view, self._vars) = (a[0], {})
        elif len(a) == 1 and isinstance(a[0], dict):
            (view, self._vars) = (None, a[0])
        else:
            (view, self._vars) = (None, {})
        self._vars.update(b)
        self._view_environment.update(self._vars)
        if view:
            import cStringIO
            (obody, oview) = (self.body, self.view)
            (self.body, self.view) = (cStringIO.StringIO(), view)
            run_view_in(self._view_environment)
            page = self.body.getvalue()
            self.body.close()
            (self.body, self.view) = (obody, oview)
        else:
            run_view_in(self._view_environment)
            page = self.body.getvalue()
        return page

    def include_meta(self):
        s = "\n";
        for meta in (self.meta or {}).iteritems():
            k, v = meta
            if isinstance(v,dict):
                s = s+'<meta'+''.join(' %s="%s"' % (xmlescape(key), xmlescape(v[key])) for key in v) +' />\n'
            else:
                s = s+'<meta name="%s" content="%s" />\n' % (k, xmlescape(v))
        self.write(s, escape=False)

    def include_files(self, extensions=None):

        """
        Caching method for writing out files.
        By default, caches in ram for 5 minutes. To change,
        response.cache_includes = (cache_method, time_expire).
        Example: (cache.disk, 60) # caches to disk for 1 minute.
        """
        from gluon import URL

        files = []
        has_js = has_css = False
        for item in self.files:
            if extensions and not item.split('.')[-1] in extensions:
                continue
            if item in files:
                continue
            if item.endswith('.js'):
                has_js = True
            if item.endswith('.css'):
                has_css = True
            files.append(item)

        if have_minify and ((self.optimize_css and has_css) or (self.optimize_js and has_js)):
            # cache for 5 minutes by default
            key = hashlib.md5(repr(files)).hexdigest()

            cache = self.cache_includes or (current.cache.ram, 60 * 5)

            def call_minify(files=files):
                return minify.minify(files,
                                     URL('static', 'temp'),
                                     current.request.folder,
                                     self.optimize_css,
                                     self.optimize_js)
            if cache:
                cache_model, time_expire = cache
                files = cache_model('response.files.minified/' + key,
                                    call_minify,
                                    time_expire)
            else:
                files = call_minify()
        s = ''
        for item in files:
            if isinstance(item, str):
                f = item.lower().split('?')[0]
                # if static_version we need also to check for
                # static_version_urls. In that case, the _.x.x.x
                # bit would have already been added by the URL()
                # function
                if self.static_version and not self.static_version_urls:
                    item = item.replace(
                        '/static/', '/static/_%s/' % self.static_version, 1)
                if f.endswith('.css'):
                    s += css_template % item
                elif f.endswith('.js'):
                    s += js_template % item
                elif f.endswith('.coffee'):
                    s += coffee_template % item
                elif f.endswith('.ts'):
                    # http://www.typescriptlang.org/
                    s += typescript_template % item
                elif f.endswith('.less'):
                    s += less_template % item
            elif isinstance(item, (list, tuple)):
                f = item[0]
                if f == 'css:inline':
                    s += css_inline % item[1]
                elif f == 'js:inline':
                    s += js_inline % item[1]
        self.write(s, escape=False)

    def stream(self,
               stream,
               chunk_size=DEFAULT_CHUNK_SIZE,
               request=None,
               attachment=False,
               filename=None
               ):
        """
        If in a controller function::

            return response.stream(file, 100)

        the file content will be streamed at 100 bytes at the time

        Args:
            stream: filename or read()able content
            chunk_size(int): Buffer size
            request: the request object
            attachment(bool): prepares the correct headers to download the file
                as an attachment. Usually creates a pop-up download window
                on browsers
            filename(str): the name for the attachment

        Note:
            for using the stream name (filename) with attachments
            the option must be explicitly set as function parameter (will
            default to the last request argument otherwise)
        """

        headers = self.headers
        # for attachment settings and backward compatibility
        keys = [item.lower() for item in headers]
        if attachment:
            if filename is None:
                attname = ""
            else:
                attname = filename
            headers["Content-Disposition"] = \
                'attachment;filename="%s"' % attname

        if not request:
            request = current.request
        if isinstance(stream, (str, unicode)):
            stream_file_or_304_or_206(stream,
                                      chunk_size=chunk_size,
                                      request=request,
                                      headers=headers,
                                      status=self.status)

        # ## the following is for backward compatibility
        if hasattr(stream, 'name'):
            filename = stream.name

        if filename and not 'content-type' in keys:
            headers['Content-Type'] = contenttype(filename)
        if filename and not 'content-length' in keys:
            try:
                headers['Content-Length'] = \
                    os.path.getsize(filename)
            except OSError:
                pass

        env = request.env
        # Internet Explorer < 9.0 will not allow downloads over SSL unless caching is enabled
        if request.is_https and isinstance(env.http_user_agent, str) and \
                not re.search(r'Opera', env.http_user_agent) and \
                re.search(r'MSIE [5-8][^0-9]', env.http_user_agent):
            headers['Pragma'] = 'cache'
            headers['Cache-Control'] = 'private'

        if request and env.web2py_use_wsgi_file_wrapper:
            wrapped = env.wsgi_file_wrapper(stream, chunk_size)
        else:
            wrapped = streamer(stream, chunk_size=chunk_size)
        return wrapped

    def download(self, request, db, chunk_size=DEFAULT_CHUNK_SIZE, attachment=True, download_filename=None):
        """
        Example of usage in controller::

            def download():
                return response.download(request, db)

        Downloads from http://..../download/filename
        """
        from pydal.exceptions import NotAuthorizedException, NotFoundException

        current.session.forget(current.response)

        if not request.args:
            raise HTTP(404)
        name = request.args[-1]
        items = re.compile('(?P<table>.*?)\.(?P<field>.*?)\..*').match(name)
        if not items:
            raise HTTP(404)
        (t, f) = (items.group('table'), items.group('field'))
        try:
            field = db[t][f]
        except AttributeError:
            raise HTTP(404)
        try:
            (filename, stream) = field.retrieve(name, nameonly=True)
        except NotAuthorizedException:
            raise HTTP(403)
        except NotFoundException:
            raise HTTP(404)
        except IOError:
            raise HTTP(404)
        headers = self.headers
        headers['Content-Type'] = contenttype(name)
        if download_filename is None:
            download_filename = filename
        if attachment:
            headers['Content-Disposition'] = \
                'attachment; filename="%s"' % download_filename.replace('"', '\"')
        return self.stream(stream, chunk_size=chunk_size, request=request)

    def json(self, data, default=None):
        if 'Content-Type' not in self.headers:
            self.headers['Content-Type'] = 'application/json'
        return json(data, default=default or custom_json)

    def xmlrpc(self, request, methods):
        """
        assuming::

            def add(a, b):
                return a+b

        if a controller function \"func\"::

            return response.xmlrpc(request, [add])

        the controller will be able to handle xmlrpc requests for
        the add function. Example::

            import xmlrpclib
            connection = xmlrpclib.ServerProxy(
                'http://hostname/app/contr/func')
            print connection.add(3, 4)

        """

        return handler(request, self, methods)

    def toolbar(self):
        from html import DIV, SCRIPT, BEAUTIFY, TAG, URL, A
        BUTTON = TAG.button
        admin = URL("admin", "default", "design", extension='html',
                    args=current.request.application)
        from gluon.dal import DAL
        dbstats = []
        dbtables = {}
        infos = DAL.get_instances()
        for k, v in infos.iteritems():
            dbstats.append(TABLE(*[TR(PRE(row[0]), '%.2fms' % (row[1]*1000))
                                   for row in v['dbstats']]))
            dbtables[k] = dict(defined=v['dbtables']['defined'] or '[no defined tables]',
                               lazy=v['dbtables']['lazy'] or '[no lazy tables]')
        u = web2py_uuid()
        backtotop = A('Back to top', _href="#totop-%s" % u)
        # Convert lazy request.vars from property to Storage so they
        # will be displayed in the toolbar.
        request = copy.copy(current.request)
        request.update(vars=current.request.vars,
                       get_vars=current.request.get_vars,
                       post_vars=current.request.post_vars)
        return DIV(
            BUTTON('design', _onclick="document.location='%s'" % admin),
            BUTTON('request',
                   _onclick="jQuery('#request-%s').slideToggle()" % u),
            BUTTON('response',
                   _onclick="jQuery('#response-%s').slideToggle()" % u),
            BUTTON('session',
                   _onclick="jQuery('#session-%s').slideToggle()" % u),
            BUTTON('db tables',
                   _onclick="jQuery('#db-tables-%s').slideToggle()" % u),
            BUTTON('db stats',
                   _onclick="jQuery('#db-stats-%s').slideToggle()" % u),
            DIV(BEAUTIFY(request), backtotop,
                _class="w2p-toolbar-hidden", _id="request-%s" % u),
            DIV(BEAUTIFY(current.session), backtotop,
                _class="w2p-toolbar-hidden", _id="session-%s" % u),
            DIV(BEAUTIFY(current.response), backtotop,
                _class="w2p-toolbar-hidden", _id="response-%s" % u),
            DIV(BEAUTIFY(dbtables), backtotop,
                _class="w2p-toolbar-hidden", _id="db-tables-%s" % u),
            DIV(BEAUTIFY(dbstats), backtotop,
                _class="w2p-toolbar-hidden", _id="db-stats-%s" % u),
            SCRIPT("jQuery('.w2p-toolbar-hidden').hide()"),
            _id="totop-%s" % u
        )


class Session(Storage):
    """
    Defines the session object and the default values of its members (None)

    - session_storage_type   : 'file', 'db', or 'cookie'
    - session_cookie_compression_level :
    - session_cookie_expires : cookie expiration
    - session_cookie_key     : for encrypted sessions in cookies
    - session_id             : a number or None if no session
    - session_id_name        :
    - session_locked         :
    - session_masterapp      :
    - session_new            : a new session obj is being created
    - session_hash           : hash of the pickled loaded session
    - session_pickled        : picked session

    if session in cookie:

    - session_data_name      : name of the cookie for session data

    if session in db:

    - session_db_record_id
    - session_db_table
    - session_db_unique_key

    if session in file:

    - session_file
    - session_filename
    """

    def connect(self,
                request=None,
                response=None,
                db=None,
                tablename='web2py_session',
                masterapp=None,
                migrate=True,
                separate=None,
                check_client=False,
                cookie_key=None,
                cookie_expires=None,
                compression_level=None
                ):
        """
        Used in models, allows to customize Session handling

        Args:
            request: the request object
            response: the response object
            db: to store/retrieve sessions in db (a table is created)
            tablename(str): table name
            masterapp(str): points to another's app sessions. This enables a
                "SSO" environment among apps
            migrate: passed to the underlying db
            separate: with True, creates a folder with the 2 initials of the
                session id. Can also be a function, e.g. ::

                    separate=lambda(session_name): session_name[-2:]

            check_client: if True, sessions can only come from the same ip
            cookie_key(str): secret for cookie encryption
            cookie_expires: sets the expiration of the cookie
            compression_level(int): 0-9, sets zlib compression on the data
                before the encryption
        """
        from gluon.dal import Field
        request = request or current.request
        response = response or current.response
        masterapp = masterapp or request.application
        cookies = request.cookies

        self._unlock(response)

        response.session_masterapp = masterapp
        response.session_id_name = 'session_id_%s' % masterapp.lower()
        response.session_data_name = 'session_data_%s' % masterapp.lower()
        response.session_cookie_expires = cookie_expires
        response.session_client = str(request.client).replace(':', '.')
        response.session_cookie_key = cookie_key
        response.session_cookie_compression_level = compression_level

        # check if there is a session_id in cookies
        try:
            old_session_id = cookies[response.session_id_name].value
        except KeyError:
            old_session_id = None
        response.session_id = old_session_id

        # if we are supposed to use cookie based session data
        if cookie_key:
            response.session_storage_type = 'cookie'
        elif db:
            response.session_storage_type = 'db'
        else:
            response.session_storage_type = 'file'
            # why do we do this?
            # because connect may be called twice, by web2py and in models.
            # the first time there is no db yet so it should do nothing
            if (global_settings.db_sessions is True
                    or masterapp in global_settings.db_sessions):
                return

        if response.session_storage_type == 'cookie':
            # check if there is session data in cookies
            if response.session_data_name in cookies:
                session_cookie_data = cookies[response.session_data_name].value
            else:
                session_cookie_data = None
            if session_cookie_data:
                data = secure_loads(session_cookie_data, cookie_key,
                                    compression_level=compression_level)
                if data:
                    self.update(data)
            response.session_id = True

        # else if we are supposed to use file based sessions
        elif response.session_storage_type == 'file':
            response.session_new = False
            response.session_file = None
            # check if the session_id points to a valid sesion filename
            if response.session_id:
                if not regex_session_id.match(response.session_id):
                    response.session_id = None
                else:
                    response.session_filename = \
                        os.path.join(up(request.folder), masterapp,
                                     'sessions', response.session_id)
                    try:
                        response.session_file = \
                            recfile.open(response.session_filename, 'rb+')
                        portalocker.lock(response.session_file,
                                         portalocker.LOCK_EX)
                        response.session_locked = True
                        self.update(pickle.load(response.session_file))
                        response.session_file.seek(0)
                        oc = response.session_filename.split('/')[-1].split('-')[0]
                        if check_client and response.session_client != oc:
                            raise Exception("cookie attack")
                    except:
                        response.session_id = None
            if not response.session_id:
                uuid = web2py_uuid()
                response.session_id = '%s-%s' % (response.session_client, uuid)
                separate = separate and (lambda session_name: session_name[-2:])
                if separate:
                    prefix = separate(response.session_id)
                    response.session_id = '%s/%s' % (prefix, response.session_id)
                response.session_filename = \
                    os.path.join(up(request.folder), masterapp,
                                 'sessions', response.session_id)
                response.session_new = True

        # else the session goes in db
        elif response.session_storage_type == 'db':
            if global_settings.db_sessions is not True:
                global_settings.db_sessions.add(masterapp)
            # if had a session on file alreday, close it (yes, can happen)
            if response.session_file:
                self._close(response)
            # if on GAE tickets go also in DB
            if settings.global_settings.web2py_runtime_gae:
                request.tickets_db = db
            if masterapp == request.application:
                table_migrate = migrate
            else:
                table_migrate = False
            tname = tablename + '_' + masterapp
            table = db.get(tname, None)
            # Field = db.Field
            if table is None:
                db.define_table(
                    tname,
                    Field('locked', 'boolean', default=False),
                    Field('client_ip', length=64),
                    Field('created_datetime', 'datetime',
                          default=request.now),
                    Field('modified_datetime', 'datetime'),
                    Field('unique_key', length=64),
                    Field('session_data', 'blob'),
                    migrate=table_migrate,
                )
                table = db[tname]  # to allow for lazy table
            response.session_db_table = table
            if response.session_id:
                # Get session data out of the database
                try:
                    (record_id, unique_key) = response.session_id.split(':')
                    record_id = long(record_id)
                except (TypeError, ValueError):
                    record_id = None

                # Select from database
                if record_id:
                    row = table(record_id, unique_key=unique_key)
                    # Make sure the session data exists in the database
                    if row:
                        # rows[0].update_record(locked=True)
                        # Unpickle the data
                        session_data = pickle.loads(row.session_data)
                        self.update(session_data)
                        response.session_new = False
                    else:
                        record_id = None
                if record_id:
                    response.session_id = '%s:%s' % (record_id, unique_key)
                    response.session_db_unique_key = unique_key
                    response.session_db_record_id = record_id
                else:
                    response.session_id = None
                    response.session_new = True
            # if there is no session id yet, we'll need to create a
            # new session
            else:
                response.session_new = True

        # set the cookie now if you know the session_id so user can set
        # cookie attributes in controllers/models
        # cookie will be reset later
        # yet cookie may be reset later
        #   Removed comparison between old and new session ids - should send
        #    the cookie all the time
        if isinstance(response.session_id, str):
            response.cookies[response.session_id_name] = response.session_id
            response.cookies[response.session_id_name]['path'] = '/'
            if cookie_expires:
                response.cookies[response.session_id_name]['expires'] = \
                    cookie_expires.strftime(FMT)

        session_pickled = pickle.dumps(self, pickle.HIGHEST_PROTOCOL)
        response.session_hash = hashlib.md5(session_pickled).hexdigest()

        if self.flash:
            (response.flash, self.flash) = (self.flash, None)

    def renew(self, clear_session=False):

        if clear_session:
            self.clear()

        request = current.request
        response = current.response
        session = response.session
        masterapp = response.session_masterapp
        cookies = request.cookies

        if response.session_storage_type == 'cookie':
            return

        # if the session goes in file
        if response.session_storage_type == 'file':
            self._close(response)
            uuid = web2py_uuid()
            response.session_id = '%s-%s' % (response.session_client, uuid)
            separate = (lambda s: s[-2:]) if session and response.session_id[2:3] == "/" else None
            if separate:
                prefix = separate(response.session_id)
                response.session_id = '%s/%s' % \
                    (prefix, response.session_id)
            response.session_filename = \
                os.path.join(up(request.folder), masterapp,
                             'sessions', response.session_id)
            response.session_new = True

        # else the session goes in db
        elif response.session_storage_type == 'db':
            table = response.session_db_table

            # verify that session_id exists
            if response.session_file:
                self._close(response)
            if response.session_new:
                return
            # Get session data out of the database
            if response.session_id is None:
                return
            (record_id, sep, unique_key) = response.session_id.partition(':')

            if record_id.isdigit() and long(record_id) > 0:
                new_unique_key = web2py_uuid()
                row = table(record_id)
                if row and row.unique_key == unique_key:
                    table._db(table.id == record_id).update(unique_key=new_unique_key)
                else:
                    record_id = None
            if record_id:
                response.session_id = '%s:%s' % (record_id, new_unique_key)
                response.session_db_record_id = record_id
                response.session_db_unique_key = new_unique_key
            else:
                response.session_new = True

    def _fixup_before_save(self):
        response = current.response
        rcookies = response.cookies
        if self._forget and response.session_id_name in rcookies:
            del rcookies[response.session_id_name]
        elif self._secure and response.session_id_name in rcookies:
            rcookies[response.session_id_name]['secure'] = True

    def clear_session_cookies(self):
        request = current.request
        response = current.response
        session = response.session
        masterapp = response.session_masterapp
        cookies = request.cookies
        rcookies = response.cookies
        # if not cookie_key, but session_data_name in cookies
        # expire session_data_name from cookies
        if response.session_data_name in cookies:
            rcookies[response.session_data_name] = 'expired'
            rcookies[response.session_data_name]['path'] = '/'
            rcookies[response.session_data_name]['expires'] = PAST
        if response.session_id_name in rcookies:
            del rcookies[response.session_id_name]

    def save_session_id_cookie(self):
        request = current.request
        response = current.response
        session = response.session
        masterapp = response.session_masterapp
        cookies = request.cookies
        rcookies = response.cookies

        # if not cookie_key, but session_data_name in cookies
        # expire session_data_name from cookies
        if not response.session_cookie_key:
            if response.session_data_name in cookies:
                rcookies[response.session_data_name] = 'expired'
                rcookies[response.session_data_name]['path'] = '/'
                rcookies[response.session_data_name]['expires'] = PAST
        if response.session_id:
            rcookies[response.session_id_name] = response.session_id
            rcookies[response.session_id_name]['path'] = '/'
            expires = response.session_cookie_expires
            if isinstance(expires, datetime.datetime):
                expires = expires.strftime(FMT)
            if expires:
                rcookies[response.session_id_name]['expires'] = expires

    def clear(self):
        # see https://github.com/web2py/web2py/issues/735
        response = current.response
        if response.session_storage_type == 'file':
            target = recfile.generate(response.session_filename)
            try:
                os.unlink(target)
            except:
                pass
        elif response.session_storage_type == 'db':
            table = response.session_db_table
            if response.session_id:
                (record_id, sep, unique_key) = response.session_id.partition(':')
                if record_id.isdigit() and long(record_id) > 0:
                    table._db(table.id == record_id).delete()
        Storage.clear(self)

    def is_new(self):
        if self._start_timestamp:
            return False
        else:
            self._start_timestamp = datetime.datetime.today()
            return True

    def is_expired(self, seconds=3600):
        now = datetime.datetime.today()
        if not self._last_timestamp or \
                self._last_timestamp + datetime.timedelta(seconds=seconds) > now:
            self._last_timestamp = now
            return False
        else:
            return True

    def secure(self):
        self._secure = True

    def forget(self, response=None):
        self._close(response)
        self._forget = True

    def _try_store_in_cookie(self, request, response):
        if self._forget or self._unchanged(response):
            # self.clear_session_cookies()
            self.save_session_id_cookie()
            return False
        name = response.session_data_name
        compression_level = response.session_cookie_compression_level
        value = secure_dumps(dict(self),
                             response.session_cookie_key,
                             compression_level=compression_level)
        rcookies = response.cookies
        rcookies.pop(name, None)
        rcookies[name] = value
        rcookies[name]['path'] = '/'
        expires = response.session_cookie_expires
        if isinstance(expires, datetime.datetime):
            expires = expires.strftime(FMT)
        if expires:
            rcookies[name]['expires'] = expires
        return True

    def _unchanged(self, response):
        session_pickled = pickle.dumps(self, pickle.HIGHEST_PROTOCOL)
        response.session_pickled = session_pickled
        session_hash = hashlib.md5(session_pickled).hexdigest()
        return response.session_hash == session_hash

    def _try_store_in_db(self, request, response):
        # don't save if file-based sessions,
        # no session id, or session being forgotten
        # or no changes to session (Unless the session is new)
        if (not response.session_db_table
                or self._forget
                or (self._unchanged(response) and not response.session_new)):
            if (not response.session_db_table
                    and global_settings.db_sessions is not True
                    and response.session_masterapp in global_settings.db_sessions):
                global_settings.db_sessions.remove(response.session_masterapp)
            # self.clear_session_cookies()
            self.save_session_id_cookie()
            return False

        table = response.session_db_table
        record_id = response.session_db_record_id
        if response.session_new:
            unique_key = web2py_uuid()
        else:
            unique_key = response.session_db_unique_key

        session_pickled = response.session_pickled or pickle.dumps(self, pickle.HIGHEST_PROTOCOL)

        dd = dict(locked=False,
                  client_ip=response.session_client,
                  modified_datetime=request.now,
                  session_data=session_pickled,
                  unique_key=unique_key)
        if record_id:
            if not table._db(table.id == record_id).update(**dd):
                record_id = None
        if not record_id:
            record_id = table.insert(**dd)
            response.session_id = '%s:%s' % (record_id, unique_key)
            response.session_db_unique_key = unique_key
            response.session_db_record_id = record_id

        self.save_session_id_cookie()
        return True

    def _try_store_in_cookie_or_file(self, request, response):
        if response.session_storage_type == 'file':
            return self._try_store_in_file(request, response)
        if response.session_storage_type == 'cookie':
            return self._try_store_in_cookie(request, response)

    def _try_store_in_file(self, request, response):
        try:
            if (not response.session_id or self._forget
                    or self._unchanged(response)):
                # self.clear_session_cookies()
                self.save_session_id_cookie()
                return False
            if response.session_new or not response.session_file:
                # Tests if the session sub-folder exists, if not, create it
                session_folder = os.path.dirname(response.session_filename)
                if not os.path.exists(session_folder):
                    os.mkdir(session_folder)
                response.session_file = recfile.open(response.session_filename, 'wb')
                portalocker.lock(response.session_file, portalocker.LOCK_EX)
                response.session_locked = True
            if response.session_file:
                session_pickled = response.session_pickled or pickle.dumps(self, pickle.HIGHEST_PROTOCOL)
                response.session_file.write(session_pickled)
                response.session_file.truncate()
        finally:
            self._close(response)

        self.save_session_id_cookie()
        return True

    def _unlock(self, response):
        if response and response.session_file and response.session_locked:
            try:
                portalocker.unlock(response.session_file)
                response.session_locked = False
            except:  # this should never happen but happens in Windows
                pass

    def _close(self, response):
        if response and response.session_file:
            self._unlock(response)
            try:
                response.session_file.close()
                del response.session_file
            except:
                pass


def pickle_session(s):
    return Session, (dict(s),)

copy_reg.pickle(Session, pickle_session)
