
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("nodejs/hello.js", "mongodb:\/\/.*\/hello_world", "mongodb://" + args.database_host + "/hello_world")
  setup_util.replace_text("nodejs/hello.js", "host: '.*',", "host: '" + args.database_host + "',")

  try:
    subprocess.check_call("npm install", shell=True, cwd="nodejs")
    subprocess.Popen("node hello.js", shell=True, cwd="nodejs")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello.js' in line:
      pid = int(line.split(None, 2)[1])
      try:
        os.kill(pid, 9)
      except OSError:
        pass
  return 0