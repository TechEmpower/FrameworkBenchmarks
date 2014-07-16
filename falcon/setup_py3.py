import subprocess
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py3/bin')

proc = None


def start(args, logfile, errfile):
    global proc
    proc = subprocess.Popen([
        bin_dir + "/gunicorn",
        "app:app",
        "-c", "gunicorn_conf.py"],
        cwd="falcon", stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc = None
    return 0
