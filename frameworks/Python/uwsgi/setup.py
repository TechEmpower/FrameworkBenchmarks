import multiprocessing
import subprocess
import os

uwsgi = os.path.expandvars('$PY2_ROOT/bin/uwsgi')
PROCS = multiprocessing.cpu_count()


def start(args, logfile, errfile):
    # --http and --http-processes create http router processes that process the
    # incoming connections and pass them to the worker processes (-p). We use
    # PROCS number of http router processes so that a single router process
    # doesn't become a bottleneck.
    subprocess.Popen(
        uwsgi + ' --master -L -l 5000 --gevent 1000 --http :8080 --http-keepalive ' +
        ' --http-processes ' + str(PROCS) + ' -p ' + str(PROCS) + ' -w hello ' +
        ' --add-header "Connection: keep-alive" ' +
        ' --pidfile /tmp/uwsgi.pid',
        shell=True, cwd="uwsgi", stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    subprocess.call(uwsgi + ' --stop /tmp/uwsgi.pid', shell=True, cwd="uwsgi", stderr=errfile, stdout=logfile)
    os.system('killall uwsgi')
    return 0
