import subprocess


def start(args, logfile, errfile):
    subprocess.Popen(
        "$PY3_GUNICORN app:app -c gunicorn_conf.py",
        cwd="falcon", stderr=errfile, stdout=logfile, shell=True)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        "kill `cat gunicorn.pid`",
        cwd="falcon", stderr=errfile, stdout=logfile, shell=True)
    return 0
