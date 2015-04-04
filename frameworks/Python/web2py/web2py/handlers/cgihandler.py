#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This file is part of the web2py Web Framework
Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)

This is a CGI handler for Apache
Requires apache+[mod_cgi or mod_cgid].

In httpd.conf put something like:

    LoadModule cgi_module modules/mod_cgi.so
    ScriptAlias / /path/to/cgihandler.py/

Example of httpd.conf ------------
<VirtualHost *:80>
  ServerName web2py.example.com
  ScriptAlias / /users/www-data/web2py/cgihandler.py/

  <Directory /users/www-data/web2py>
    AllowOverride None
    Order Allow,Deny
    Deny from all
    <Files cgihandler.py>
      Allow from all
    </Files>
  </Directory>

  AliasMatch ^/([^/]+)/static/(.*) \
           /users/www-data/web2py/applications/$1/static/$2
  <Directory /users/www-data/web2py/applications/*/static/>
    Order Allow,Deny
    Allow from all
  </Directory>

  <Location /admin>
  Deny from all
  </Location>

  <LocationMatch ^/([^/]+)/appadmin>
  Deny from all
  </LocationMatch>

  CustomLog /private/var/log/apache2/access.log common
  ErrorLog /private/var/log/apache2/error.log
</VirtualHost>
----------------------------------

"""

import os
import sys
import wsgiref.handlers

path = os.path.dirname(os.path.abspath(__file__))
os.chdir(path)

if not os.path.isdir('applications'):
    raise RuntimeError('Running from the wrong folder')

sys.path = [path] + [p for p in sys.path if not p == path]

import gluon.main

wsgiref.handlers.CGIHandler().run(gluon.main.wsgibase)
