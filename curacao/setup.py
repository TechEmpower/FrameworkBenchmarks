
import subprocess
import sys
import time
import os

def start(args, logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call('"..\\sbt\\sbt.bat" ";start;shell"', shell=True, cwd="curacao", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call('"../sbt/sbt ";start;shell"', shell=True, cwd="curacao", stderr=errfile, stdout=logfile)
    
  time.sleep(5)
  return 0

def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%curacao-benchmark%'\" call terminate", stderr=errfile, stdout=logfile)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'curacao-benchmark' in line:
        try:
          pid = int(line.split(None, 2)[1])
          os.kill(pid, 15)
        except OSError:
          pass
  
  return 0
