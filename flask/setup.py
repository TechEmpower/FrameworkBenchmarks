import subprocess
import os
import time

proc = None


def start(args, logfile, errfile):
    global proc
    proc = subprocess.Popen(
        "exec $PY2_GUNICORN app:app -c gunicorn_conf.py",
        cwd="flask", stderr=errfile, stdout=logfile, shell=True)
    return 0


def stop(logfile, errfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    proc = None
    time.sleep(1)

    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'gunicorn' in line:
        errfile.write("Killing: " + line + "\n")
        pid = int(line.split()[1])
        os.kill(pid, 15)
    return 0
