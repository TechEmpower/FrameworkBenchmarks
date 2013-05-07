
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("express/app.js", "mongodb:\/\/.*\/hello_world", "mongodb://" + args.database_host + "/hello_world")
  setup_util.replace_text("express/app.js", "localhost", args.database_host)

  try:
    npm()
    if os.name == 'nt':
      subprocess.Popen("set NODE_ENV=production", shell=True)
      subprocess.Popen("node app", shell=True, cwd="express")
    else:
      subprocess.Popen("NODE_ENV=production node app", shell=True, cwd="express")
    return 0
  except subprocess.CalledProcessError:
    return 1

def npm():
  if os.name == 'nt':
    subprocess.check_call("copy package.json package.json.dist /y > NUL", shell=True, cwd="express")
    setup_util.replace_text("express/package.json", ".*mysql.*", "")
    setup_util.replace_text("express/package.json", ".*mapper.*", "")
  
  try:
    subprocess.check_call("npm install", shell=True, cwd="express")
  finally:
    if os.name == 'nt':
      subprocess.check_call("del package.json", shell=True, cwd="express")
      subprocess.check_call("ren package.json.dist package.json", shell=True, cwd="express")

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
