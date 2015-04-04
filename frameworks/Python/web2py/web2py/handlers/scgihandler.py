#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
scgihandler.py - handler for SCGI protocol

Modified by Michele Comitini <michele.comitini@glisco.it>
from fcgihandler.py to support SCGI

fcgihandler has the following copyright:
" This file is part of the web2py Web Framework
  Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
  License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
"

This is a handler for lighttpd+scgi
This file has to be in the PYTHONPATH
Put something like this in the lighttpd.conf file:

server.document-root="/var/www/web2py/"
# for >= linux-2.6
server.event-handler = "linux-sysepoll"

url.rewrite-once = (
      "^(/.+?/static/.+)$" => "/applications$1",
      "(^|/.*)$" => "/handler_web2py.scgi$1",
)
scgi.server = ( "/handler_web2py.scgi" =>
                  ("handler_web2py" =>
                      ( "host" => "127.0.0.1",
                        "port" => "4000",
                        "check-local" => "disable", # don't forget to set "disable"!
                      )
    )
)




"""

LOGGING = False
SOFTCRON = False

import sys
import os

path = os.path.dirname(os.path.abspath(__file__))
os.chdir(path)

if not os.path.isdir('applications'):
    raise RuntimeError('Running from the wrong folder')

sys.path = [path] + [p for p in sys.path if not p == path]

import gluon.main

# uncomment one of the two imports below depending on the SCGIWSGI server installed
#import paste.util.scgiserver as scgi
from wsgitools.scgi.forkpool import SCGIServer
from wsgitools.filters import WSGIFilterMiddleware, GzipWSGIFilter

wsgiapp = WSGIFilterMiddleware(gluon.main.wsgibase, GzipWSGIFilter)

if LOGGING:
    application = gluon.main.appfactory(wsgiapp=wsgiapp,
                                        logfilename='httpserver.log',
                                        profiler_dir=None)
else:
    application = wsgiapp

if SOFTCRON:
    from gluon.settings import global_settings
    global_settings.web2py_crontype = 'soft'

# uncomment one of the two rows below depending on the SCGIWSGI server installed
#scgi.serve_application(application, '', 4000).run()
SCGIServer(application, port=4000).enable_sighandler().run()
