import os
import subprocess
import sys
import time


bin_dir = os.path.expanduser('~/FrameworkBenchmarks/installs/py2/bin')
python = os.path.expanduser(os.path.join(bin_dir, 'python'))
pip = os.path.expanduser(os.path.join(bin_dir, 'pip'))
cwd = os.path.expanduser('~/FrameworkBenchmarks/tornado')


def start(args, logfile, errfile):
    subprocess.call(pip + ' install -r requirements.txt', cwd=cwd, shell=True, stderr=errfile, stdout=logfile)

    subprocess.Popen(
        python + ' server.py --port=8080 --postgres=%s --logging=error' % (args.database_host,),
        shell=True, cwd=cwd, stderr=errfile, stdout=logfile)
    return 0


def stop(logfile, errfile):
    for line in subprocess.check_output(['ps', 'aux']).splitlines():
        if 'server.py --port=8080' in line:
            pid = int(line.split(None, 2)[1])
            os.kill(pid, 9)
    return 0

if __name__ == '__main__':
    class DummyArg:
        database_host = 'localhost'
    start(DummyArg(), sys.stderr, sys.stderr)
    time.sleep(1)
    stop(sys.stderr, sys.stderr)
