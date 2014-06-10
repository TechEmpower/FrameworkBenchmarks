import subprocess
import sys
import setup_util
from os.path import expanduser
import os

home = expanduser("~")

def start(args, logfile, errfile):
    try:
        subprocess.check_call("mvn clean package;", shell=True, cwd="dropwizard", stderr=errfile, stdout=logfile)
        subprocess.Popen("java -jar target/dropwizard-mongodb-0.0.1-SNAPSHOT.jar server hello-world.yml", shell=True, cwd="dropwizard", stderr=errfile, stdout=logfile)
        return 0
    except subprocess.CalledProcessError:
        return 1
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello-world' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
