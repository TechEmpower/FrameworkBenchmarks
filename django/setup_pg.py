import subprocess
import setup_util
import multiprocessing
import os

home = os.path.expanduser('~')
bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
NCPU = multiprocessing.cpu_count()

proc = None


def start(args):
    global proc
    setup_util.replace_text("django/hello/hello/settings.py", "HOST': '.*'", "HOST': '" + args.database_host + "'")
    setup_util.replace_text("django/hello/hello/settings.py", "\/home\/ubuntu",  home)
    env = os.environ.copy()
    env['DJANGO_DB'] = 'postgresql_psycopg2'
    proc = subprocess.Popen([
        bin_dir + "/gunicorn",
        "hello.wsgi:application",
        "-k", "meinheld.gmeinheld.MeinheldWorker",
        "-b", "0.0.0.0:8080",
        '-w', str(NCPU*3),
        "--log-level=critical"],
        cwd="django/hello",
        env=env)
    return 0

def stop():
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc.wait()
    proc = None
    return 0
