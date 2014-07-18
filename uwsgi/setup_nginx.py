import subprocess
import multiprocessing
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
config_dir = os.path.expanduser('~/FrameworkBenchmarks/config')
NCPU = multiprocessing.cpu_count()
NGINX_COMMAND = 'sudo /usr/local/nginx/sbin/nginx -c ' + config_dir + '/nginx_uwsgi.conf'


def start(args, logfile, errfile):
    try:
        subprocess.check_call(
            NGINX_COMMAND,
            shell=True, stdout=logfile, stderr=errfile)

        # Run in the background, but keep stdout/stderr for easy debugging.
        # Note that this uses --gevent 1000 just like setup.py.
        subprocess.Popen(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi.ini' +
            ' --processes ' + str(NCPU) +
            ' --gevent 1000 --wsgi hello',
            shell=True, cwd='uwsgi',
            stdout=logfile, stderr=errfile)
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile, errfile):
    subprocess.check_call(
        NGINX_COMMAND + ' -s stop',
        shell=True, stdout=logfile, stderr=errfile)

    subprocess.call(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi_stop.ini', shell=True, stdout=logfile, stderr=errfile)

    os.system('killall nginx')
    os.system('killall uwsgi')
    return 0
