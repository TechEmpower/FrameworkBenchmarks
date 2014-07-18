import subprocess
import os

BIN = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')

proc = None


def start(args, logfile, errfile):
    global proc
    proc = subprocess.Popen(
        [BIN + "/gunicorn",
         "-c", "gunicorn_conf.py",
         "-e", "DBHOSTNAME=" + args.database_host,
         "app:app"],
        cwd="bottle", stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc.wait()
    proc = None
    return 0
