import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  subprocess.Popen('leda app.moon', shell=True, cwd="leda", stderr=errfile, stdout=logfile)

  return 0

def stop(logfile, errfile):
  
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'app.moon' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)
      except OSError:
        pass


  return 0
