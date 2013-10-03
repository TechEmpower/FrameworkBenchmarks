import subprocess
import multiprocessing
import os
import setup_util

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
config_dir = os.path.expanduser('~/FrameworkBenchmarks/config')
NCPU = multiprocessing.cpu_count()

def start(args):
    setup_util.replace_text("bottle/app.py", "DBHOSTNAME", args.database_host)
    try:
        subprocess.check_call('sudo /usr/local/nginx/sbin/nginx -c ' +
            config_dir + '/nginx_uwsgi.conf', shell=True)
        # Run in the background, but keep stdout/stderr for easy debugging
        subprocess.Popen(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi.ini' +
            ' --processes ' + str(NCPU * 3) +
            ' --wsgi app:app',
            shell=True, cwd='bottle')
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop():
    subprocess.call('sudo /usr/local/nginx/sbin/nginx -s stop', shell=True)
    subprocess.call(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi_stop.ini', shell=True)
    return 0
