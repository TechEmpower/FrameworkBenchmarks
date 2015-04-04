#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
| This file is part of the web2py Web Framework
| Copyrighted by Massimo Di Pierro <mdipierro@cs.depaul.edu>
| License: LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
"""

import os
import sys
import platform
from gluon.storage import Storage

global_settings = Storage()
settings = global_settings  # legacy compatibility

if not hasattr(os, 'mkdir'):
    global_settings.db_sessions = True

if global_settings.db_sessions is not True:
    global_settings.db_sessions = set()

global_settings.gluon_parent = \
    os.environ.get('web2py_path', os.getcwd())

global_settings.applications_parent = global_settings.gluon_parent

global_settings.app_folders = set()

global_settings.debugging = False

global_settings.is_pypy = \
    hasattr(platform, 'python_implementation') and \
    platform.python_implementation() == 'PyPy'

global_settings.is_jython = \
    'java' in sys.platform.lower() or \
    hasattr(sys, 'JYTHON_JAR') or \
    str(sys.copyright).find('Jython') > 0

global_settings.is_source = os.path.exists(os.path.join(
        global_settings.gluon_parent, 'web2py.py'))
