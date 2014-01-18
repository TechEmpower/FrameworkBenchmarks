import subprocess
import multiprocessing
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
config_dir = os.path.expanduser('~/FrameworkBenchmarks/config')
NCPU = multiprocessing.cpu_count()

def start(args, logfile, errfile):
    try:
        subprocess.check_call('sudo /usr/local/nginx/sbin/nginx -c ' +
            config_dir + '/nginx_uwsgi.conf', shell=True)
        # Run in the background, but keep stdout/stderr for easy debugging
        subprocess.Popen(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi.ini' +
            ' --processes ' + str(NCPU) +
            ' --wsgi hello:app',
            shell=True, cwd='wsgi',
            stdout=logfile, stderr=errfile)
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile, errfile):
    subprocess.call('sudo /usr/local/nginx/sbin/nginx -s stop', shell=True, stdout=logfile, stderr=errfile)
    subprocess.call(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi_stop.ini', shell=True, stdout=logfile, stderr=errfile)
    return 0
