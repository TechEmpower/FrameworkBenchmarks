import subprocess
import os


proc = None


def start(args, logfile, errfile):
    global proc
    proc = subprocess.Popen(
            "exec $PY3_GUNICORN -c gunicorn_conf.py app:app",
            cwd="bottle", shell=True, stderr=errfile, stdout=logfile)
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
