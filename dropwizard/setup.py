import subprocess
import sys
import setup_util
from os.path import expanduser
import os

home = expanduser("~")

def start(args, logfile):
    setup_util.replace_text("dropwizard/hello-world.yml", "url: jdbc:mysql://.*/hello_world", "url: jdbc:mysql://" + args.database_host + ":3306/hello_world")

    try:
        subprocess.check_call("mvn clean package;", shell=True, cwd="dropwizard")
        subprocess.Popen("java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world.yml", shell=True, cwd="dropwizard")
        return 0
    except subprocess.CalledProcessError:
        return 1
def stop(logfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello-world' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
