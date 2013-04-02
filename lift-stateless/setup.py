
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("lift-stateless/src/main/resources/props/default.props", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")

  subprocess.Popen("./sbt update container:start", shell=True, cwd="lift-stateless")

  return 0
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if './start' in line or ('play' in line and 'java' in line):
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  try:
    os.remove("play-scala/RUNNING_PID")
  except OSError:
    pass

  return 0
