
import subprocess

def start(args, logfile, errfile):
  try:
    subprocess.check_call("bundle install", shell=True, cwd="gwan", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo ./gwan_linux64-bit/gwan -d", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  try:
    subprocess.call("sudo ./gwan_linux64-bit/gwan -k", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
