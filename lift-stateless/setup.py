
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("lift-stateless/src/main/scala/Main.scala", "> \".*:3306", "> \"" + args.database_host + ":3306")

  subprocess.check_call("./sbt update assembly", shell=True, cwd="lift-stateless")
  subprocess.Popen("./run", shell=True, cwd="lift-stateless")

  return 0
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if './sbt' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  try:
    os.remove("lift-stateless/RUNNING_PID")
  except OSError:
    pass

  return 0
