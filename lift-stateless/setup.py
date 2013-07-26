
import subprocess
import sys
import setup_util
import os
import time

def start(args):
  setup_util.replace_text("lift-stateless/src/main/scala/Main.scala", "> \".*:3306", "> \"" + args.database_host + ":3306")

  if os.name == 'nt':
    subprocess.check_call('"..\\sbt\\sbt.bat" update assembly', shell=True, cwd="lift-stateless")
    subprocess.Popen(".\\run.bat", shell=True, cwd="lift-stateless")  
  else:
    subprocess.check_call("../sbt/sbt update assembly", shell=True, cwd="lift-stateless")
    subprocess.Popen("./run", shell=True, cwd="lift-stateless")

  time.sleep(5)
  return 0
def stop():
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%lift-stateless-assembly%'\" call terminate")
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'lift-stateless-assembly' in line and 'java' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
  return 0
