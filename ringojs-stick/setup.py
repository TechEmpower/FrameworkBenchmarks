
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("ringojs-stick/views.js", "dbHost = '.*';", "dbHost = '" + args.database_host + "';")

  try:
    subprocess.Popen("ringo --production ringo-main.js", shell=True, cwd="ringojs-stick")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'ringo-main.js' in line:
      pid = int(line.split(None, 2)[1])
      try:
        os.kill(pid, 9)
      except OSError:
        pass
  return 0
