from __future__ import print_function
import os
import subprocess
import time

CWD = os.path.dirname(__file__)

import multiprocessing

def start(args, logfile, errfile):
    print("Envoronment variables:")
    for k, v in os.environ.items():
        print('%s=%s' % (k, v))
    print("Cores:", multiprocessing.cpu_count())
    subprocess.Popen(
        "$PY2_GUNICORN app:app -c gunicorn_conf.py",
        cwd=CWD, stderr=errfile, stdout=logfile, shell=True)
    time.sleep(3)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        "kill `cat gunicorn.pid`",
        cwd=CWD, stderr=errfile, stdout=logfile, shell=True)
    time.sleep(3)
    return 0
