import subprocess
import multiprocessing
import os


CWD = os.path.abspath(os.path.dirname(__file__))
bin_dir = os.path.expandvars('$PY2_ROOT/bin')
NCPU = multiprocessing.cpu_count()
NGINX_COMMAND = 'sudo /usr/local/nginx/sbin/nginx -c ' + CWD + '/nginx.conf'


def start(args, logfile, errfile):
    try:
        subprocess.check_call(
            NGINX_COMMAND,
            shell=True, stdout=logfile, stderr=errfile)

        # Run in the background, but keep stdout/stderr for easy debugging
        subprocess.Popen(
            bin_dir + '/uwsgi --ini uwsgi.ini' +
            ' --processes ' + str(NCPU) +
            ' --wsgi hello:app',
            shell=True, cwd=CWD, stdout=logfile, stderr=errfile)
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile, errfile):
    subprocess.check_call(
        NGINX_COMMAND + ' -s stop',
        shell=True, stdout=logfile, stderr=errfile)

    subprocess.call(
        bin_dir + '/uwsgi --stop /tmp/uwsgi.pid',
        shell=True, cwd=CWD, stderr=errfile, stdout=logfile)
    return 0
