
import subprocess
import sys
import time
import os

def start(args, logfile):


    if os.name == 'nt':
      subprocess.check_call("..\\sbt\\sbt.bat update compile", shell=True, cwd="finagle", stderr=logfile, stdout=logfile)
      subprocess.Popen("..\\sbt\\sbt.bat -Ddb.host=" + args.database_host + " run", cwd="finagle", shell=True, stderr=logfile, stdout=logfile)
    else:
      subprocess.check_call("../sbt/sbt update compile", shell=True, cwd="finagle", stderr=logfile, stdout=logfile)
      subprocess.Popen("../sbt/sbt -Ddb.host=" + args.database_host + " run", cwd="finagle", shell=True, stderr=logfile, stdout=logfile)

    time.sleep(5)
    return 0



def stop(logfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%\\\\sbt\\\\sbt%'\" call terminate", stderr=logfile, stdout=logfile)
  else:
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
