# This is the laziest glue code I've ever written.  I used the HttpListener
# setup.py/setup.ps1 as a reference.
from subprocess import check_call

from os.path import (
    join,
    abspath,
    dirname,
    normpath,
)

basedir = abspath(dirname(__file__))
setup_ps1 = join(basedir, 'setup.ps1')

start_cmd = "powershell -Command \"%s\" start" % setup_ps1
stop_cmd  = "powershell -Command \"%s\" stop"  % setup_ps1
cwd = 'C:\\PyParallel33'

def start(args, logfile, errfile):
    check_call(start_cmd, cwd=cwd, stderr=errfile, stdout=logfile)
    return 0

def stop(logfile, errfile):
    check_call(stop_cmd, cwd=cwd, stderr=errfile, stdout=logfile)
    return 0

if __name__ == '__main__':
    import sys
    from subprocess import PIPE
    if 'stop' in sys.argv:
        stop(PIPE, PIPE)
    else:
        start(None, PIPE, PIPE)
