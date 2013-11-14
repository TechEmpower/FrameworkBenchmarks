import subprocess
import setup_util
import multiprocessing
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
NCPU = multiprocessing.cpu_count()

proc = None


def start(args, logfile, errfile):
    global proc
    proc = subprocess.Popen([
        bin_dir + "/gunicorn",
        "hello:app",
        "-k", "meinheld.gmeinheld.MeinheldWorker",
        "-b", "0.0.0.0:8080",
        '-w', str(NCPU),
        "--log-level=critical"],
        cwd="wsgi", stderr=errfile, stdout=logfile)
    return 0

def stop(logfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc.wait()
    proc = None
    return 0
