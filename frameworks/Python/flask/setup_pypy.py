import os
import subprocess
import time

CWD = os.path.dirname(__file__)


def start(args, logfile, errfile):
    subprocess.Popen(
        "$PYPY_GUNICORN app:app -c gunicorn_conf.py",
        cwd=CWD, stderr=errfile, stdout=logfile, shell=True)
    time.sleep(3)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        "kill `cat gunicorn.pid`",
        cwd=CWD, stderr=errfile, stdout=logfile, shell=True)
    time.sleep(3)
    return 0
