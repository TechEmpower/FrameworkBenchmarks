import subprocess
import os
import time

proc = None


def start(args, logfile, errfile):
    global proc
    proc = subprocess.Popen(
        "exec $PYPY_GUNICORN app:app -c gunicorn_conf.py",
        cwd="flask", stderr=errfile, stdout=logfile, shell=True)
    return 0


def stop(logfile, errfile):
    global proc
    if proc is None:
        return 0
    proc.terminate()
    for _ in range(10):
        if proc.poll() is not None:
            break
        time.sleep(0.2)

    if proc.poll() is None:
        proc.kill()

    proc = None

    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'gunicorn' in line:
        errfile.write("Killing: " + line + "\n")
        pid = int(line.split()[1])
        os.kill(pid, 15)
    return 0

