import subprocess
import os

bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')

proc = None


def start(args, logfile, errfile):
    global proc
    proc = subprocess.Popen([
        bin_dir + "/gunicorn",
        "app:app",
        "-c", "gunicorn_conf.py"],
        cwd="falcon", stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    global proc
    if proc:
        proc.terminate()
        proc = None

    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'FrameworkBenchmarks/installs/py2/bin/' in line:
        errfile.write("Killing: " + line + "\n")
        pid = int(line.split()[1])
        os.kill(pid, 15)
    return 0
