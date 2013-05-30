import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("hapi/app.js", "localhost", args.database_host)

  try:
    npm()
    if os.name == 'nt':
      subprocess.Popen("set NODE_ENV=production", shell=True)
      subprocess.Popen("node app", shell=True, cwd="hapi")
    else:
      subprocess.Popen("NODE_ENV=production node app", shell=True, cwd="hapi")
    return 0
  except subprocess.CalledProcessError:
    return 1

def npm():
  if os.name == 'nt':
    subprocess.check_call("copy package.json package.json.dist /y > NUL", shell=True, cwd="hapi")
    setup_util.replace_text("hapi/package.json", ".*mapper.*", "")

  try:
    subprocess.check_call("npm install", shell=True, cwd="hapi")
  finally:
    if os.name == 'nt':
      subprocess.check_call("del package.json", shell=True, cwd="hapi")
      subprocess.check_call("ren package.json.dist package.json", shell=True, cwd="hapi")

def stop():
  if os.name == 'nt':
    subprocess.Popen("taskkill /f /im node.exe > NUL", shell=True)
    return 0

  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'node app' in line:
      pid = int(line.split(None, 2)[1])
      try:
        os.kill(pid, 9)
      except OSError:
        pass
  return 0
