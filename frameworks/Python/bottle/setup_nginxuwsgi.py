import subprocess
import multiprocessing
import os
import setup_util


bin_dir = os.path.expandvars('$TROOT/py2/bin')
NCPU = multiprocessing.cpu_count()
NGINX_COMMAND = 'sudo /usr/local/nginx/sbin/nginx -c $TROOT/nginx.conf'


def start(args, logfile, errfile):
    setup_util.replace_text('bottle/app.py', 'database_host', args.database_host)
    try:
        subprocess.call(
            NGINX_COMMAND,
            shell=True, stdout=logfile, stderr=errfile)

        # Run in the background, but keep stdout/stderr for easy debugging
        subprocess.Popen(
            "{0}/uwsgi --socket /tmp/uwsgi.sock --ini uwsgi.ini --processes {1} --wsgi app:app --listen 100".format(
                bin_dir, NCPU*3),
            shell=True, cwd=args.troot, stderr=errfile, stdout=logfile)
        return 0
    except subprocess.CalledProcessError:
        return 1


def stop(logfile, errfile):
    subprocess.call(
        NGINX_COMMAND + ' -s stop',
        shell=True, stdout=logfile, stderr=errfile)

    subprocess.call(bin_dir + '/uwsgi --stop /tmp/uwsgi.pid',
                    shell=True, stderr=errfile, stdout=logfile)
    return 0
