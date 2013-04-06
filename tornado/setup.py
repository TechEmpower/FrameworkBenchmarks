import subprocess
import sys
import setup_util
import os
from os.path import expanduser

home = expanduser("~")
cwd = "%s/FrameworkBenchmarks/tornado" % home


def start(args):
    setup_util.replace_text(
        cwd + "/server.py", "127.0.0.1", args.database_host)

    subprocess.check_call("pip install -r %s/requirements.txt")

    subprocess.Popen("python %s/FrameworkBenchmarks/tornado/server.py --port=8000 --logging=error" % home, shell=True, cwd=cwd)
    subprocess.Popen("python %s/FrameworkBenchmarks/tornado/server.py --port=8001 --logging=error" % home, shell=True, cwd=cwd)
    subprocess.Popen("python %s/FrameworkBenchmarks/tornado/server.py --port=8002 --logging=error" % home, shell=True, cwd=cwd)
    subprocess.Popen("python %s/FrameworkBenchmarks/tornado/server.py --port=8003 --logging=error" % home, shell=True, cwd=cwd)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/php/deploy/nginx.conf", shell=True)

    return 0


def stop():

    try:

        subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)

    except subprocess.CalledProcessError:
        #TODO: Better handle exception.
        pass

    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
        if 'server.py' in line:
            try:
                pid = int(line.split(None, 2)[1])
                os.kill(pid, 9)
            except OSError:
                pass

    return 0
