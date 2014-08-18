import subprocess
import time


def start(args, logfile, errfile):
    subprocess.Popen(
        "$PY2_GUNICORN --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=postgresql_psycopg2",
        cwd="django", shell=True, stderr=errfile, stdout=logfile)
    time.sleep(3)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        "kill `cat gunicorn.pid`",
        cwd="django", shell=True, stderr=errfile, stdout=logfile)
    time.sleep(3)
    return 0
