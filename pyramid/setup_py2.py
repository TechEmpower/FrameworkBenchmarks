import subprocess
import setup_util
import multiprocessing
import os

home = os.path.expanduser('~')
pyramid_dir = home + "/FrameworkBenchmarks/pyramid"
bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
NCPU = multiprocessing.cpu_count()

proc = None


def start(args, logfile, errfile):
    global proc
    setup_util.replace_text(
        pyramid_dir + "/frameworkbenchmarks/models.py",
        "DBHOSTNAME = 'localhost'",
        "DBHOSTNAME = '%s'" % args.database_host
    )
    subprocess.call(bin_dir + '/pip install -e .', cwd='pyramid', shell=True, stderr=errfile, stdout=logfile)
    proc = subprocess.Popen([
        bin_dir + '/gunicorn',
        'wsgi:app',
        '-b', "0.0.0.0:6543",
        '-w', str(NCPU*3)],
        cwd='pyramid', stderr=errfile, stdout=logfile
    )
    return 0

def stop(logfile, errfile):
    global proc
    if proc is not None:
        proc.terminate()
        proc.wait()
    return 0
