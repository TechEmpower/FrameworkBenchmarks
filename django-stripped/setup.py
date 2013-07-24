import subprocess
import setup_util
import multiprocessing
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
NCPU = multiprocessing.cpu_count()

proc = None


def start(args):
    global proc
    setup_util.replace_text("django-stripped/hello/hello/settings.py", "HOST': '.*'", "HOST': '" + args.database_host + "'")
    proc = subprocess.Popen([
        bin_dir + "/gunicorn",
        "hello.wsgi:application",
        "-k", "meinheld.gmeinheld.MeinheldWorker",
        "-b", "0.0.0.0:8080",
        '-w', str(NCPU*3),
        "--log-level=critical"],
        cwd="django-stripped/hello")
    return 0

def stop():
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc = None
    return 0
