import subprocess
import sys
import setup_util
import os
from os.path import expanduser

home = expanduser("~")
cwd = "%s/FrameworkBenchmarks/tornado" % home


def start(args):
    setup_util.replace_text(
        cwd + "/server.py", "localhost", args.database_host)

    subprocess.Popen("python %s/FrameworkBenchmarks/tornado/server.py --port=8080 --logging=error" % home, shell=True, cwd=cwd)
    return 0


def stop():
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
        if 'server.py' in line:
            #try:
            pid = int(line.split(None, 2)[1])
            os.kill(pid, 9)
            #except OSError:
            #    pass

    return 0
