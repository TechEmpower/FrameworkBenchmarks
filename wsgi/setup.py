import subprocess
import setup_util

proc = None


def start(args):
    proc = subprocess.Popen(
        "gunicorn hello:app -k meinheld.gmeinheld.MeinheldWorker -b 0.0.0.0:8080 -w " +
        str((args.max_threads * 2)) + " --preload --log-level=critical", shell=True, cwd="wsgi")
    return 0

def stop():
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc = None
    return 0
