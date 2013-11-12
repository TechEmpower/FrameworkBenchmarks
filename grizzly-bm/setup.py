import subprocess
import sys
import setup_util
import os

def start(args, logfile):
  try:
    subprocess.check_call("mvn clean compile assembly:single", shell=True, cwd="grizzly-bm")
    subprocess.Popen("java -jar grizzly-bm-0.1-jar-with-dependencies.jar".rsplit(" "), cwd="grizzly-bm/target")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'grizzly-bm' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0