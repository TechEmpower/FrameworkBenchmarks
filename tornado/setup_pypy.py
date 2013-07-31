import subprocess
import setup_util
from os.path import expanduser

python = expanduser('~/FrameworkBenchmarks/installs/pypy/bin/python')
cwd = expanduser('~/FrameworkBenchmarks/tornado')
proc = None


def start(args):
    global proc
    setup_util.replace_text(
        cwd + "/server.py", "localhost", args.database_host)

    proc = subprocess.Popen(
        python + " server.py --port=8080 --logging=error",
        shell=True, cwd=cwd)
    return 0

def stop():
    global proc
    if proc:
        proc.terminate()
        proc = None
    return 0
