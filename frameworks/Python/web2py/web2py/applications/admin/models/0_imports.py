import time
import os
import sys
import re
import urllib
import cgi
import difflib
import shutil
import stat
import socket

from textwrap import dedent

try:
    from mercurial import ui, hg, cmdutil
    try:
        from mercurial.scmutil import addremove
    except:
        from mercurial.cmdutil import addremove
    have_mercurial = True
except ImportError:
    have_mercurial = False

from gluon.utils import md5_hash
from gluon.fileutils import listdir, cleanpath, up
from gluon.fileutils import tar, tar_compiled, untar, fix_newlines
from gluon.languages import findT, update_all_languages
from gluon.myregex import *
from gluon.restricted import *
from gluon.compileapp import compile_application, remove_compiled_application
