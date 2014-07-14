import subprocess
import multiprocessing
import os
import setup_util

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
config_dir = os.path.expanduser('~/FrameworkBenchmarks/config')
NCPU = multiprocessing.cpu_count()
NGINX_COMMAND = '/usr/local/nginx/sbin/nginx -c ' + config_dir + '/nginx_uwsgi.conf'


def start(args, logfile, errfile):
    setup_util.replace_text("flask/app.py", "DBHOSTNAME", args.database_host)
    try:
        subprocess.call(
            NGINX_COMMAND,
            shell=True, stdout=logfile, stderr=errfile)

        # Run in the background, but keep stdout/stderr for easy debugging
        subprocess.Popen(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi.ini' +
            ' --processes ' + str(NCPU * 3) +
            ' --wsgi app:app',
            shell=True, cwd='flask', stderr=errfile, stdout=logfile)
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile, errfile):
    subprocess.call(
        NGINX_COMMAND + ' -s stop',
        shell=True, stdout=logfile, stderr=errfile)
    subprocess.call(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi_stop.ini', shell=True, stderr=errfile, stdout=logfile)

    os.system('killall nginx')
    os.system('killall uwsgi')
    return 0
