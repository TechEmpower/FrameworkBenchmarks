import subprocess
import sys
import setup_util
import os

def start(args, logfile):
  if os.name != 'nt':
    return 1
  
  try:
    setup_util.replace_text("servicestack/svc/SelfHost/App.config", "localhost", args.database_host)
    subprocess.check_call("powershell -Command .\\setup_self.ps1 start", cwd="servicestack", stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile):
  if os.name != 'nt':
    return 0
  
  subprocess.check_call("powershell -Command .\\setup_self.ps1 stop", cwd="servicestack", stderr=logfile, stdout=logfile)
  return 0