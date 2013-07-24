import subprocess
import sys
import setup_util
import os

uwsgi = os.path.expanduser('~/FrameworkBenchmarks/installs/py3/bin/uwsgi')


def start(args):
    subprocess.Popen(uwsgi + ' -L --gevent 1000 --http :8080 -w hello --pidfile /tmp/uwsgi.pid', shell=True, cwd="uwsgi")
    return 0


def stop():
    try:
        subprocess.Popen(uwsgi + ' --stop /tmp/uwsgi.pid', shell=True, cwd="uwsgi")
    except OSError:
        pass
    return 0
