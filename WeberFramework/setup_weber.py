import sys
import subprocess

def start(args, logfile, errfile):
    try:
        subprocess.check_call("mix deps.get", cwd="WeberFramework", shell=True, stderr=errfile, stdout=logfile)
        subprocess.check_call("yes | mix compile --all --force", cwd="WeberFramework", shell=True, stderr=errfile, stdout=logfile)
        subprocess.check_call("./start.sh", cwd="WeberFramework", shell=True, stderr=errfile, stdout=logfile)
        return 0
    except subprocess.CalledProcessError:
        return 1
 
def stop(logfile, errfile):
    try:
        subprocess.check_call("killall beam.smp", shell=True, cwd="/usr/bin")
        return 0
    except subprocess.CalledProcessError:
        return 1
