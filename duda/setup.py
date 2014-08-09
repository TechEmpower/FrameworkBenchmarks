import subprocess
import sys
import os
import setup_util 

def start(args, logfile, errfile):
  subprocess.Popen("dudac -w $TROOT/webservice -p 2001", shell=True, stderr=errfile, stdout=logfile);
  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'monkey' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
