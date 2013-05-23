import subprocess
import sys
import setup_util
import os

def start(args):
  try:
    subprocess.check_call("mvn clean compile assembly:single", shell=True, cwd="undertow")
    subprocess.Popen("java -jar undertow-example-0.1-jar-with-dependencies.jar".rsplit(" "), cwd="undertow/target")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'undertow-example' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0