import os
import subprocess

CWD = os.path.dirname(__file__)


def start(args, logfile, errfile):
    subprocess.Popen(
        "$PY2_GUNICORN wsgi:app -c gunicorn_conf.py",
        cwd=CWD, stderr=errfile, stdout=logfile, shell=True)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        "kill `cat gunicorn.pid`",
        cwd=CWD, stderr=errfile, stdout=logfile, shell=True)
    return 0
