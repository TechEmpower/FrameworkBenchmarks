import os
import subprocess
import setup_util

CWD = os.path.dirname(__file__)


def start(args, logfile, errfile):
    setup_util.replace_text('bottle/app.py', 'database_host', args.database_host)
    subprocess.Popen(
        "$PY3_GUNICORN app:app -c gunicorn_conf.py",
        cwd=CWD, stderr=errfile, stdout=logfile, shell=True)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        "kill `cat gunicorn.pid`",
        cwd=CWD, stderr=errfile, stdout=logfile, shell=True)
    return 0
