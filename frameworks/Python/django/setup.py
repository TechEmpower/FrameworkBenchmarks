import subprocess
import setup_util


def start(args, logfile, errfile):
    setup_util.replace_text('django/hello/hello/settings.py', 'database_host', args.database_host)
    subprocess.Popen(
        "$PY2_GUNICORN --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=mysql",
        cwd="django", shell=True, stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    subprocess.call(
        "kill `cat gunicorn.pid`",
        cwd="django", shell=True, stderr=errfile, stdout=logfile)
    return 0
