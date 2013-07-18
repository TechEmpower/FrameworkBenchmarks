
import subprocess
import sys
import time
import os

def start(args=None):


    
    subprocess.check_call("../sbt/sbt update compile", shell=True, cwd="finagle")
    subprocess.Popen("../sbt/sbt -Ddb.host=" + args.database_host + " run", cwd="finagle", shell=True)
    time.sleep(5)
    return 0



def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'sbt' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
      except OSError:
        pass
  
  return 0
