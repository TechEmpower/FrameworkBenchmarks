import subprocess
import os

CWD = os.path.abspath(os.path.dirname(__file__))


def start(args, logfile, errfile):
    subprocess.Popen(
        "$PY2_GUNICORN wsgi:app -c gunicorn_conf.py",
        cwd=CWD, shell=True, stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        "kill `cat gunicorn.pid`",
        cwd=CWD, shell=True, stderr=errfile, stdout=logfile)
    return 0
