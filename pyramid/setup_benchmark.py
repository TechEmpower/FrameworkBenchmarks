import subprocess
import setup_util
import multiprocessing
import os

home = os.path.expanduser('~')
bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py3/bin')
NCPU = multiprocessing.cpu_count()

proc = None


def start(args):
    global proc
    setup_util.replace_text(
        "frameworkbenchmarks/models.py",
        "DBHOSTNAME = 'localhost'",
        "DBHOSTNAME = '%s'" % args.database_host
    )
    proc = subprocess.Popen([
        bin_dir + '/gunicorn',
        'wsgi:app',
        '-b', "0.0.0.0:6543",
        '-w', str(NCPU*3)],
        cwd='pyramid'
    )
    return 0

def stop():
    global proc
    if proc is not None:
        proc.terminate()
        proc.wait()
    return 0
