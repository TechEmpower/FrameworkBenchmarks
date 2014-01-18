import subprocess
import setup_util
import multiprocessing
import os

home = os.path.expanduser('~')
bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py3/bin')
NCPU = multiprocessing.cpu_count()

proc = None


def start(args, logfile, errfile):
    global proc
    setup_util.replace_text("django/hello/hello/settings.py", "HOST': '.*'", "HOST': '" + args.database_host + "'")
    setup_util.replace_text("django/hello/hello/settings.py", "\/home\/ubuntu",  home)
    env = os.environ.copy()
    env['DJANGO_DB'] = 'mysql'
    proc = subprocess.Popen([
        bin_dir + "/gunicorn",
        "hello.wsgi:application",
        "-k", "meinheld.gmeinheld.MeinheldWorker",
        "-b", "0.0.0.0:8080",
        '-w', str(NCPU*3),
        "--log-level=critical"],
        cwd="django/hello",
        env=env, stderr=errfile, stdout=logfile)
    return 0

def stop(logfile, errfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc.wait()
    proc = None
    return 0
