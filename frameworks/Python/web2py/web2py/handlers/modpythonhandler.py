#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of the web2py Web Framework
Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

WSGI wrapper for mod_python. Requires Python 2.2 or greater.
Part of CherryPy mut modified by Massimo Di Pierro (2008) for web2py

<Location /myapp>
    SetHandler python-program
    PythonHandler modpythonhandler
    PythonPath \"['/path/to/web2py/'] + sys.path\"
    PythonOption SCRIPT_NAME /myapp
</Location>

Some WSGI implementations assume that the SCRIPT_NAME environ variable will
always be equal to 'the root URL of the app'; Apache probably won't act as
you expect in that case. You can add another PythonOption directive to tell
modpython_gateway to force that behavior:

    PythonOption SCRIPT_NAME /mcontrol

The module.function will be called with no arguments on server shutdown,
once for each child process or thread.
"""

import traceback
import sys
import os
from mod_python import apache

path = os.path.dirname(os.path.abspath(__file__))
os.chdir(path)

if not os.path.isdir('applications'):
    raise RuntimeError('Running from the wrong folder')

sys.path = [path] + [p for p in sys.path if not p == path]

import gluon.main


class InputWrapper(object):
    """ Input wrapper for the wsgi handler  """

    def __init__(self, req):
        """ InputWrapper constructor  """

        self.req = req

    def close(self):
        """ """

        pass

    def read(self, size=-1):
        """ Wrapper for req.read  """

        return self.req.read(size)

    def readline(self, size=-1):
        """ Wrapper for req.readline  """

        return self.req.readline(size)

    def readlines(self, hint=-1):
        """ Wrapper for req.readlines  """

        return self.req.readlines(hint)

    def __iter__(self):
        """ Defines a generator with the req data  """

        line = self.readline()
        while line:
            yield line

            # Notice this won't prefetch the next line; it only
            # gets called if the generator is resumed.
            line = self.readline()


class ErrorWrapper(object):
    """ Error wrapper for the wsgi handler  """

    def __init__(self, req):
        """ ErrorWrapper constructor """

        self.req = req

    def flush(self):
        """   """

        pass

    def write(self, msg):
        """ Logs the given msg in the log file """

        self.req.log_error(msg)

    def writelines(self, seq):
        """ Writes various lines in the log file """

        self.write(''.join(seq))


bad_value = "You must provide a PythonOption '%s', either 'on' or 'off', when running a version of mod_python < 3.1"


class Handler:
    """ Defines the handler  """

    def __init__(self, req):
        """ Handler constructor  """

        self.started = False
        options = req.get_options()

        # Threading and forking
        try:
            q = apache.mpm_query
            threaded = q(apache.AP_MPMQ_IS_THREADED)
            forked = q(apache.AP_MPMQ_IS_FORKED)
        except AttributeError:
            threaded = options.get('multithread', '').lower()

            if threaded == 'on':
                threaded = True
            elif threaded == 'off':
                threaded = False
            else:
                raise ValueError(bad_value % 'multithread')

            forked = options.get('multiprocess', '').lower()

            if forked == 'on':
                forked = True
            elif forked == 'off':
                forked = False
            else:
                raise ValueError(bad_value % 'multiprocess')

        env = self.environ = dict(apache.build_cgi_env(req))

        if 'SCRIPT_NAME' in options:
            # Override SCRIPT_NAME and PATH_INFO if requested.
            env['SCRIPT_NAME'] = options['SCRIPT_NAME']
            env['PATH_INFO'] = req.uri[len(options['SCRIPT_NAME']):]

        env['wsgi.input'] = InputWrapper(req)
        env['wsgi.errors'] = ErrorWrapper(req)
        env['wsgi.version'] = (1, 0)
        env['wsgi.run_once'] = False

        if env.get('HTTPS') in ('yes', 'on', '1'):
            env['wsgi.url_scheme'] = 'https'
        else:
            env['wsgi.url_scheme'] = 'http'

        env['wsgi.multithread'] = threaded
        env['wsgi.multiprocess'] = forked

        self.request = req

    def run(self, application):
        """ Run the application  """

        try:
            result = application(self.environ, self.start_response)

            for data in result:
                self.write(data)

            if not self.started:
                self.request.set_content_length(0)

            if hasattr(result, 'close'):
                result.close()
        except:
            traceback.print_exc(None, self.environ['wsgi.errors'])

            if not self.started:
                self.request.status = 500
                self.request.content_type = 'text/plain'
                data = 'A server error occurred. Please contact the ' + \
                       'administrator.'
                self.request.set_content_length(len(data))
                self.request.write(data)

    def start_response(self, status, headers, exc_info=None):
        """ Defines the request data  """

        if exc_info:
            try:
                if self.started:
                    raise exc_info[0], exc_info[1], exc_info[2]
            finally:
                exc_info = None

        self.request.status = int(status[:3])

        for (key, val) in headers:
            if key.lower() == 'content-length':
                self.request.set_content_length(int(val))
            elif key.lower() == 'content-type':
                self.request.content_type = val
            else:
                self.request.headers_out.add(key, val)

        return self.write

    def write(self, data):
        """ Write the request data  """

        if not self.started:
            self.started = True

        self.request.write(data)


def handler(req):
    """ Execute the gluon app  """

    Handler(req).run(gluon.main.wsgibase)
    return apache.OK
