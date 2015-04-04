USAGE = """
from web2py main folder
python scripts/make_min_web2py.py /path/to/minweb2py

it will mkdir minweb2py and build a minimal web2py installation
- no admin, no examples, one line welcome
- no scripts
- drops same rarely used contrib modules
- more modules could be dropped but minimal difference
"""

# files to include from top level folder (default.py will be rebuilt)
REQUIRED = """
VERSION
web2py.py
anyserver.py
applications/__init__.py
applications/welcome/controllers/default.py
handlers/fcgihandler.py
handlers/gaehandler.py
handlers/wsgihandler.py
"""

# files and folders to exclude from gluon folder (comment with # if needed)
IGNORED = """
gluon/contrib/websocket_messaging.py
gluon/contrib/feedparser.py
gluon/contrib/generics.py
gluon/contrib/gql.py
gluon/contrib/populate.py
gluon/contrib/sms_utils.py
gluon/contrib/spreadsheet.py
gluon/tests/
gluon/contrib/markdown/
gluon/contrib/pyfpdf/
gluon/contrib/pymysql/
gluon/contrib/pyrtf/
gluon/contrib/pysimplesoap/
"""

import sys
import os
import shutil
import glob


def main():
    global REQUIRED, IGNORED

    if len(sys.argv) < 2:
        print USAGE

    # make target folder
    target = sys.argv[1]
    os.mkdir(target)

    # change to os specificsep
    REQUIRED = REQUIRED.replace('/', os.sep)
    IGNORED = IGNORED.replace('/', os.sep)

    # make a list of all files to include
    files = [x.strip() for x in REQUIRED.split('\n')
             if x and not x[0] == '#']
    ignore = [x.strip() for x in IGNORED.split('\n')
              if x and not x[0] == '#']

    def accept(filename):
        for p in ignore:
            if filename.startswith(p):
                return False
        return True
    pattern = os.path.join('gluon', '*.py')
    while True:
        newfiles = [x for x in glob.glob(pattern) if accept(x)]
        if not newfiles:
            break
        files += newfiles
        pattern = os.path.join(pattern[:-3], '*.py')
    # copy all files, make missing folder, build default.py
    files.sort()
    defaultpy = os.path.join(
        'applications', 'welcome', 'controllers', 'default.py')
    for f in files:
        dirs = f.split(os.path.sep)
        for i in range(1, len(dirs)):
            try:
                os.mkdir(target + os.sep + os.path.join(*dirs[:i]))
            except OSError:
                pass
        if f == defaultpy:
            open(os.path.join(
                target, f), 'w').write('def index(): return "hello"\n')
        else:
            shutil.copyfile(f, os.path.join(target, f))

if __name__ == '__main__':
    main()
