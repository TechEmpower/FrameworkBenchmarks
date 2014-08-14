
import subprocess
import sys
import setup_util
import os
import time

def start(args, logfile, errfile):
  setup_util.replace_text("lift-stateless/src/main/scala/Main.scala", "> \".*:3306", "> \"" + args.database_host + ":3306")

  if os.name == 'nt':
    subprocess.check_call('"..\\sbt\\sbt.bat" update assembly', shell=True, cwd="lift-stateless", stderr=errfile, stdout=logfile)
    subprocess.Popen(".\\run.bat", shell=True, cwd="lift-stateless", stderr=errfile, stdout=logfile)  
  else:
    subprocess.check_call("../sbt/sbt update assembly", shell=True, cwd="lift-stateless", stderr=errfile, stdout=logfile)
    subprocess.Popen("./run", shell=True, cwd="lift-stateless", stderr=errfile, stdout=logfile)

  time.sleep(5)
  return 0
def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%lift-stateless-assembly%'\" call terminate", stderr=errfile, stdout=logfile)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'lift-stateless-assembly' in line and 'java' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)

  # Takes up so much disk space
  if os.name == 'nt':
    subprocess.check_call("del /f /s /q target", shell=True, cwd="lift-stateless", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("rm -rf target", shell=True, cwd="lift-stateless", stderr=errfile, stdout=logfile)
    
  return 0
