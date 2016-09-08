# -*- coding: utf-8 -*-
import sys
import os

path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'web2py')

sys.path = [path] + [p for p in sys.path if not p == path]

from gluon.compileapp import compile_application

compile_application(os.path.join(path, 'applications', 'standard'))
compile_application(os.path.join(path, 'applications', 'optimized'))
