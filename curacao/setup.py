
import subprocess
import sys
import os

def start(args, logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call('"..\\sbt\\sbt.bat" ";start;shell"', shell=True, cwd="curacao", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call('../sbt/sbt ";start;shell"', shell=True, cwd="curacao", stderr=errfile, stdout=logfile)
    
  return 0

def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%sbt-launch%'\" call terminate", stderr=errfile, stdout=logfile)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'sbt-launch' in line:
        try:
          pid = int(line.split(None, 2)[1])
          os.kill(pid, 15)
        except OSError:
          pass
  
  return 0

##start([], open('log.out','a'), open('error.out','a'))
##stop(open('log.out','a'), open('error.out','a'))

