import subprocess
import os


def start(args, logfile, errfile):
    subprocess.Popen("go run hello.go".rsplit(" "), cwd="gin", stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
        if 'hello' in line:
            pid = int(line.split(None, 2)[1])
            os.kill(pid, 15)
    return 0
