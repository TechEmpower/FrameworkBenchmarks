import sys
import subprocess

def start(args, logfile, errfile):
    try:
        subprocess.check_call("mix deps.get", shell=True, stderr=errfile, stdout=logfile)
        subprocess.check_call("mix compile --all --force", shell=True, stderr=errfile, stdout=logfile)
        subprocess.check_call("./start.sh", shell=True, stderr=errfile, stdout=logfile)
        return 0
    except subprocess.CalledProcessError:
        return 1
 
def stop(logfile, errfile):
    try:
        subprocess.check_call("killall beam.smp", shell=True, cwd="/usr/bin")
        return 0
    except subprocess.CalledProcessError:
        return 1