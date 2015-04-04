#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of the web2py Web Framework
Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
"""

##############################################################################
# Configuration parameters for Google App Engine
##############################################################################
LOG_STATS = False      # web2py level log statistics
APPSTATS = True         # GAE level usage statistics and profiling
DEBUG = False          # debug mode
#
# Read more about APPSTATS here
#   http://googleappengine.blogspot.com/2010/03/easy-performance-profiling-with.html
# can be accessed from:
#   http://localhost:8080/_ah/stats
##############################################################################
# All tricks in this file developed by Robin Bhattacharyya
##############################################################################


import time
import os
import sys
import logging
import cPickle
import pickle
import wsgiref.handlers
import datetime

path = os.path.dirname(os.path.abspath(__file__))

# os.chdir(path) ?

if not os.path.isdir('applications'):
    raise RuntimeError('Running from the wrong folder')

sys.path = [path] + [p for p in sys.path if not p == path]

sys.modules['cPickle'] = sys.modules['pickle']


from gluon.settings import global_settings
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app


global_settings.web2py_runtime_gae = True
global_settings.db_sessions = True
if os.environ.get('SERVER_SOFTWARE', '').startswith('Devel'):
    (global_settings.web2py_runtime, DEBUG) = \
        ('gae:development', True)
else:
    (global_settings.web2py_runtime, DEBUG) = \
        ('gae:production', False)


import gluon.main


def log_stats(fun):
    """Function that will act as a decorator to make logging"""
    def newfun(env, res):
        """Log the execution time of the passed function"""
        timer = lambda t: (t.time(), t.clock())
        (t0, c0) = timer(time)
        executed_function = fun(env, res)
        (t1, c1) = timer(time)
        log_info = """**** Request: %.2fms/%.2fms (real time/cpu time)"""
        log_info = log_info % ((t1 - t0) * 1000, (c1 - c0) * 1000)
        logging.info(log_info)
        return executed_function
    return newfun


logging.basicConfig(level=logging.INFO)


def wsgiapp(env, res):
    """Return the wsgiapp"""
    env['PATH_INFO'] = env['PATH_INFO'].decode('latin1').encode('utf8')

    #when using the blobstore image uploader GAE dev SDK passes these as unicode
    # they should be regular strings as they are parts of URLs
    env['wsgi.url_scheme'] = str(env['wsgi.url_scheme'])
    env['QUERY_STRING'] = str(env['QUERY_STRING'])
    env['SERVER_NAME'] = str(env['SERVER_NAME'])

    #this deals with a problem where GAE development server seems to forget
    # the path between requests
    if global_settings.web2py_runtime == 'gae:development':
        gluon.admin.create_missing_folders()

    web2py_path = global_settings.applications_parent  # backward compatibility

    return gluon.main.wsgibase(env, res)


if LOG_STATS or DEBUG:
    wsgiapp = log_stats(wsgiapp)


def main():
    """Run the wsgi app"""
    run_wsgi_app(wsgiapp)

if __name__ == '__main__':
    main()
