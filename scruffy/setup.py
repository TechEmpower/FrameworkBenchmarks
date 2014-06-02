
import subprocess
import sys
import time
import os

def start(args, logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call('"..\\sbt\\sbt.bat" assembly', shell=True, cwd="scruffy", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("../sbt/sbt assembly", shell=True, cwd="scruffy", stderr=errfile, stdout=logfile)
    
  subprocess.Popen("java -jar target/scala-2.11/scruffy-benchmark-assembly-1.0.jar", cwd="scruffy", shell=True,
                   stderr=errfile, stdout=logfile)
  time.sleep(5)
  return 0

def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%scruffy-benchmark%'\" call terminate", stderr=errfile, stdout=logfile)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'scruffy-benchmark' in line:
        try:
          pid = int(line.split(None, 2)[1])
          os.kill(pid, 15)
        except OSError:
          pass
  
  return 0
