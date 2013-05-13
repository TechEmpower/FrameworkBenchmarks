import subprocess
import sys
import setup_util
import os

def start(args):
  try:
    subprocess.check_call("mvn clean package", shell=True, cwd="grizzly-jersey")
    subprocess.Popen(("java -jar target/grizzly-jersey-example.jar -dbhost " + args.database_host).rsplit(" "), cwd="grizzly-jersey")
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'grizzly-jersey' in line and 'jar' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
