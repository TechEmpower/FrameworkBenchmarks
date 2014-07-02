import subprocess
import setup_util
import multiprocessing
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
NCPU = multiprocessing.cpu_count()

CIRCUS_INI = """\
[watcher:app]
cmd = {BIN}/chaussette --fd=$(circus.sockets.app) --backend=meinheld hello.app
use_sockets = True
numprocesses = {PROCS}

[socket:app]
host = 0.0.0.0
port = 8080
"""

proc = None


def start(args, logfile, errfile):
    global proc

    subprocess.check_call(bin_dir + "/pip install -r requirements.txt",
                          cwd="wsgi", stderr=errfile, stdout=logfile, shell=True)

    with open("wsgi/circus.ini", "w") as f:
        f.write(CIRCUS_INI.format(BIN=bin_dir, PROCS=NCPU*3))

    proc = subprocess.Popen([bin_dir + "/circusd", "circus.ini"],
		            cwd="wsgi", stderr=errfile, stdout=logfile)
    return 0

def stop(logfile, errfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc.wait()
    proc = None
    return 0
