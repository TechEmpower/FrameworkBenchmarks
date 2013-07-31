import multiprocessing
import subprocess
import sys
import setup_util
import os
import time

uwsgi = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin/uwsgi')
PROCS = multiprocessing.cpu_count()

def start(args):
    subprocess.Popen(
        uwsgi + ' --master -L --gevent 1000 --http :8080 --http-keepalive ' +
        '-p ' + str(PROCS) + ' -w hello --add-header "Connection: keep-alive" ' +
        ' --pidfile /tmp/uwsgi.pid',
        shell=True, cwd="uwsgi")
    return 0


def stop():
    try:
        subprocess.Popen(uwsgi + ' --stop /tmp/uwsgi.pid', shell=True, cwd="uwsgi")
    except OSError:
        pass
    time.sleep(1)
    return 0
