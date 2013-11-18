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
        "app:app",
        "-k", "meinheld.gmeinheld.MeinheldWorker",
        "-b", "0.0.0.0:8080",
        '-w', str(NCPU*3),
        "--log-level=critical"],
        cwd="falcon", stderr=errfile, stdout=logfile)
    return 0

def stop(logfile, errfile):
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'FrameworkBenchmarks/installs/py2/bin/' in line:
        pid = int(line.split(None,2)[1])
        os.kill(pid, 9)
    return 0
