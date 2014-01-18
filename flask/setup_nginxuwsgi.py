import subprocess
import multiprocessing
import os
import setup_util

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
config_dir = os.path.expanduser('~/FrameworkBenchmarks/config')
NCPU = multiprocessing.cpu_count()

def start(args, logfile, errfile):
    setup_util.replace_text("flask/app.py", "DBHOSTNAME", args.database_host)
    try:
        subprocess.check_call('sudo /usr/local/nginx/sbin/nginx -c ' +
            config_dir + '/nginx_uwsgi.conf', shell=True, stderr=errfile, stdout=logfile)
        # Run in the background, but keep stdout/stderr for easy debugging
        subprocess.Popen(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi.ini' +
            ' --processes ' + str(NCPU * 3) +
            ' --wsgi app:app',
            shell=True, cwd='flask', stderr=errfile, stdout=logfile)
        return 0
    except subprocess.CalledProcessError:
        return 1

def stop(logfile, errfile):
    subprocess.call('sudo /usr/local/nginx/sbin/nginx -s stop', shell=True, stderr=errfile, stdout=logfile)
    subprocess.call(bin_dir + '/uwsgi --ini ' + config_dir + '/uwsgi_stop.ini', shell=True, stderr=errfile, stdout=logfile)

    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'FrameworkBenchmarks/installs/py2/bin/' in line:
        pid = int(line.split(None,2)[1])
        os.kill(pid, 9)

    return 0
