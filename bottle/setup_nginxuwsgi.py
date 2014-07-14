import subprocess
import multiprocessing
import os


bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
config_dir = os.path.expanduser('~/FrameworkBenchmarks/config')
NCPU = multiprocessing.cpu_count()
NGINX_COMMAND = 'sudo /usr/local/nginx/sbin/nginx -c ' + config_dir + '/nginx_uwsgi.conf'


def start(args, logfile, errfile):
    try:
        subprocess.call(
            NGINX_COMMAND,
            shell=True, stdout=logfile, stderr=errfile)

        # Run in the background, but keep stdout/stderr for easy debugging
        subprocess.Popen(
            "{0}/uwsgi --ini {1}/uwsgi.ini --processes {2} --env DBHOSTNAME={3} --wsgi app:app".format(
                bin_dir, config_dir, NCPU*3, args.database_host),
            shell=True, cwd='bottle', stderr=errfile, stdout=logfile)

        return 0
    except subprocess.CalledProcessError:
        return 1


def stop(logfile, errfile):
    subprocess.call(
        NGINX_COMMAND + ' -s stop',
        shell=True, stdout=logfile, stderr=errfile)

    subprocess.call(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi_stop.ini',
                    shell=True, stderr=errfile, stdout=logfile)
    return 0
