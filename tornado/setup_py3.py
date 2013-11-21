import subprocess
import setup_util
from os.path import expanduser

python = expanduser('~/FrameworkBenchmarks/installs/py3/bin/python3')
cwd = expanduser('~/FrameworkBenchmarks/tornado')


def start(args, logfile, errfile):
    setup_util.replace_text(
        cwd + "/server.py", "localhost", args.database_host)

    subprocess.Popen(
        python + " server.py --port=8080 --logging=error",
        shell=True, cwd=cwd, stderr=errfile, stdout=logfile)
    return 0

def stop(logfile, errfile):
    for line in subprocess.check_output("ps aux"):
        if "installs/py3/bin/python3" in line:
            pid = int(line.split(None,2)[1])
            os.kill(pid, 9)
    return 0
