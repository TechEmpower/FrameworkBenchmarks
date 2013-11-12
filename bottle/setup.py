import subprocess
import setup_util
import multiprocessing
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
NCPU = multiprocessing.cpu_count()

proc = None


def start(args, logfile):
    global proc
    setup_util.replace_text("bottle/app.py", "DBHOSTNAME", args.database_host)
    proc = subprocess.Popen([
        bin_dir + "/gunicorn",
        "app:app",
        "-k", "meinheld.gmeinheld.MeinheldWorker",
        "-b", "0.0.0.0:8080",
        '-w', str(NCPU*3),
        "--log-level=critical"],
        cwd="bottle", stderr=logfile, stdout=logfile)
    return 0

def stop(logfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc.wait()
    proc = None
    return 0
