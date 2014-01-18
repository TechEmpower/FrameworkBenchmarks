from os.path import expanduser
from os import kill
import subprocess
import sys
import time


python = expanduser('~/FrameworkBenchmarks/installs/pypy/bin/pypy')
cwd = expanduser('~/FrameworkBenchmarks/tornado')


def start(args, logfile, errfile):
    subprocess.Popen(
        python + " server.py --port=8080 --mongo=%s --logging=error" % (args.database_host,),
        shell=True, cwd=cwd, stderr=errfile, stdout=logfile)
    return 0

def stop(logfile, errfile):
    for line in subprocess.check_output(["ps", "aux"]).splitlines():
        if 'server.py --port=8080' in line:
            pid = int(line.split(None,2)[1])
            kill(pid, 9)
    return 0

if __name__ == '__main__':
    class DummyArg:
        database_host = 'localhost'
    start(DummyArg(), sys.stderr, sys.stderr)
    time.sleep(1)
    stop(sys.stderr, sys.stderr)
