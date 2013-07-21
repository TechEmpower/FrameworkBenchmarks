import subprocess
import setup_util
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')

proc = None


def start(args):
    setup_util.replace_text("flask/app.py", "DBHOSTNAME", args.database_host)
    proc = subprocess.Popen(
        bin_dir + "/gunicorn app:app -k meinheld.gmeinheld.MeinheldWorker -b 0.0.0.0:8080 -w " +
        str((args.max_threads * 2)) + " --preload --log-level=critical", shell=True, cwd="flask")
    return 0

def stop():
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc = None
    return 0
