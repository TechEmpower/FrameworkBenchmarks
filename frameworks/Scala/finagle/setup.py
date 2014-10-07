
import subprocess
import sys
import time
import os

def start(args, logfile, errfile):

    if os.name == 'nt':
      subprocess.check_call("..\\sbt\\sbt.bat update compile", shell=True, cwd="finagle", stderr=errfile, stdout=logfile)
      subprocess.Popen("..\\sbt\\sbt.bat -Ddb.host=" + args.database_host + " run", cwd="finagle", shell=True, stderr=errfile, stdout=logfile)
    else:
      subprocess.check_call("sbt update compile", shell=True, cwd="finagle", stderr=errfile, stdout=logfile)
      subprocess.Popen("sbt -Ddb.host=" + args.database_host + " run", cwd="finagle", shell=True, stderr=errfile, stdout=logfile)

    return 0


def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%\\\\sbt\\\\sbt%'\" call terminate", stderr=errfile, stdout=logfile)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'sbt' in line:
        try:
          pid = int(line.split(None, 2)[1])
          os.kill(pid, 15)
        except OSError:
          pass
  
  return 0
