import subprocess
import os

BIN = os.path.expanduser('~/FrameworkBenchmarks/installs/py3/bin')

proc = None


def start(args, logfile, errfile):
    global proc
    proc = subprocess.Popen( "$PY3_GUNICORN -c gunicorn_conf.py -e DBHOSTNAME=%s app:app" % args.database_host, cwd="bottle", shell=True, stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc.wait()
    proc = None

    subprocess.call("sudo pkill gunicorn", shell=True, stderr=errfile, stdout=logfile)

    return 0
