import sys
import os

path = os.path.dirname(os.path.abspath(__file__))
os.chdir(path)

if not os.path.isdir('applications'):
    raise RuntimeError('Running from the wrong folder')

sys.path = [path] + [p for p in sys.path if not p == path]

sys.stdout = sys.stderr

from gluon.main import wsgibase
application = wsgibase
