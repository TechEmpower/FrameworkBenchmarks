
import subprocess
import sys
import time
import os

def start(args=None, logfile):
  if os.name == 'nt':
    subprocess.check_call('"..\\sbt\\sbt.bat" assembly', shell=True, cwd="spray")
  else:
    subprocess.check_call("../sbt/sbt assembly", shell=True, cwd="spray")
    
  subprocess.Popen("java -jar target/scala-2.10/spray-benchmark-assembly-1.0.jar", cwd="spray", shell=True)  
  time.sleep(5)
  return 0

def stop(logfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%spray-benchmark%'\" call terminate")
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'spray-benchmark' in line:
        try:
          pid = int(line.split(None, 2)[1])
          os.kill(pid, 9)
        except OSError:
          pass
  
  return 0
