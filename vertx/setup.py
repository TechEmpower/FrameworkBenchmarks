
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("vertx/App.groovy", "host: '.*'", "host: '" + args.database_host + "'")

  try:
    subprocess.check_call("vertx run WebServer.java", shell=True, cwd="vertx")
    #subprocess.Popen("vertx run App.groovy -repo vert-x.github.io", shell=True, cwd="vertx")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'App.groovy' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)

  return 0
