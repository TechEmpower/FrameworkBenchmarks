import subprocess
import multiprocessing
import os


CWD = os.path.abspath(os.path.dirname(__file__))
uwsgi = os.path.expandvars('$PY2_ROOT/bin/uwsgi')
NCPU = multiprocessing.cpu_count()
NGINX_COMMAND = 'sudo /usr/local/nginx/sbin/nginx -c ' + CWD + '/nginx.conf'


def start(args, logfile, errfile):
    try:
        subprocess.check_call(
            NGINX_COMMAND,
            shell=True, stdout=logfile, stderr=errfile)

        # Run in the background, but keep stdout/stderr for easy debugging.
        # Note that this uses --gevent 1000 just like setup.py.
        subprocess.Popen(
            uwsgi + ' --ini uwsgi.ini' +
            ' --processes ' + str(NCPU) +
            ' --gevent 1000 --wsgi hello',
            shell=True, cwd='uwsgi',
            stdout=logfile, stderr=errfile)
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile, errfile):
    subprocess.call(
        NGINX_COMMAND + ' -s stop',
        shell=True, stdout=logfile, stderr=errfile)

    subprocess.call(
        uwsgi + ' --stop /tmp/uwsgi.pid',
        shell=True, cwd="uwsgi", stderr=errfile, stdout=logfile)

    os.system('killall nginx')
    os.system('killall uwsgi')
    return 0
