import os
import subprocess
import sys
import time

CWD = os.path.dirname(__file__)


def start(args, logfile, errfile):
    subprocess.Popen(
        "$PY3 server.py --port=8080 --mongo=%s --logging=error" % (args.database_host,),
        shell=True, cwd=CWD, stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        'pgrep -f server.py',
        shell=True, stderr=errfile, stdout=logfile)
    subprocess.call(
        'pkill -f server.py',
        shell=True, stderr=errfile, stdout=logfile)
    time.sleep(1)


if __name__ == '__main__':
    class DummyArg:
        database_host = 'localhost'
    start(DummyArg(), sys.stderr, sys.stderr)
    time.sleep(1)
    stop(sys.stderr, sys.stderr)
