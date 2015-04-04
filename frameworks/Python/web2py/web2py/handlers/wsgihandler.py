#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of the web2py Web Framework
Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)


This is a WSGI handler for Apache
Requires apache+mod_wsgi.

In httpd.conf put something like:

    LoadModule wsgi_module modules/mod_wsgi.so
    WSGIScriptAlias / /path/to/wsgihandler.py

"""

# change these parameters as required
LOGGING = False
SOFTCRON = False

import sys
import os

path = os.path.dirname(os.path.abspath(__file__))
os.chdir(path)

if not os.path.isdir('applications'):
    raise RuntimeError('Running from the wrong folder')

sys.path = [path] + [p for p in sys.path if not p == path]

sys.stdout = sys.stderr

import gluon.main

if LOGGING:
    application = gluon.main.appfactory(wsgiapp=gluon.main.wsgibase,
                                        logfilename='httpserver.log',
                                        profiler_dir=None)
else:
    application = gluon.main.wsgibase

if SOFTCRON:
    from gluon.settings import global_settings
    global_settings.web2py_crontype = 'soft'
