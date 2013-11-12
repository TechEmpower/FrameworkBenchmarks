import subprocess
import multiprocessing
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
config_dir = os.path.expanduser('~/FrameworkBenchmarks/config')
NCPU = multiprocessing.cpu_count()

def start(args, logfile):
    try:
        subprocess.check_call('sudo /usr/local/nginx/sbin/nginx -c ' +
            config_dir + '/nginx_uwsgi.conf', shell=True)
        # Run in the background, but keep stdout/stderr for easy debugging.
        # Note that this uses --gevent 1000 just like setup.py.
        subprocess.Popen(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi.ini' +
            ' --processes ' + str(NCPU) +
            ' --gevent 1000 --wsgi hello',
            shell=True, cwd='uwsgi')
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile):
    subprocess.call('sudo /usr/local/nginx/sbin/nginx -s stop', shell=True)
    subprocess.call(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi_stop.ini', shell=True)
    return 0
