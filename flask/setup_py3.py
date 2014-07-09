import subprocess
import setup_util
import multiprocessing
import os

PY2BIN = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
PY3BIN = os.path.expanduser('~/FrameworkBenchmarks/installs/py3/bin')
NCPU = multiprocessing.cpu_count()

CIRCUS_INI = """\
[watcher:app]
cmd = {BIN}/chaussette --fd=$(circus.sockets.app) --backend=meinheld app.app
use_sockets = True
numprocesses = {PROCS}

[socket:app]
host = 0.0.0.0
port = 8080
"""

proc = None


def start(args, logfile, errfile):
    global proc

    subprocess.check_call(PY3BIN + "/pip3 install -r requirements.txt",
                          cwd="flask", stderr=errfile, stdout=logfile, shell=True)

    with open("flask/circus.ini", "w") as f:
        f.write(CIRCUS_INI.format(BIN=PY3BIN, PROCS=NCPU*3))

    setup_util.replace_text("flask/app.py", "DBHOSTNAME", args.database_host)
    proc = subprocess.Popen([PY2BIN + "/circusd", "circus.ini"],
		            cwd="flask", stderr=errfile, stdout=logfile)
    return 0

def stop(logfile, errfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc = None
    return 0
