import subprocess
import setup_util

proc = None


def start(args):
    global proc
    setup_util.replace_text("flask/app.py", "DBHOSTNAME", args.database_host)
    proc = subprocess.Popen(
        "~/FrameworkBenchmarks/installs/pypy-2.0.2/bin/gunicorn " +
        "app:app -k tornado -b 0.0.0.0:8080 -w " +
        str((args.max_threads * 2)) + " --preload --log-level=critical", shell=True, cwd="flask")
    return 0

def stop():
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc = None
    return 0
