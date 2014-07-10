import subprocess
import setup_util
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')

proc = None


def start(args, logfile, errfile):
    global proc
    setup_util.replace_text("bottle/app.py", "DBHOSTNAME", args.database_host)
    proc = subprocess.Popen(
        [bin_dir + "/gunicorn", "-c", "gunicorn_conf.py", "app:app"],
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
