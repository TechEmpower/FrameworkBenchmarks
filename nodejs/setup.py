
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("nodejs/hello.js", "mongodb:\/\/.*\/hello_world", "mongodb://" + args.database_host + "/hello_world")
  setup_util.replace_text("nodejs/hello.js", "localhost", args.database_host)

  try:
    npm()
    subprocess.Popen("node hello.js", shell=True, cwd="nodejs")
    return 0
  except subprocess.CalledProcessError:
    return 1

def npm():
  if os.name == 'nt':
    subprocess.check_call("copy package.json package.json.dist /y > NUL", shell=True, cwd="nodejs")
    setup_util.replace_text("nodejs/package.json", ".*mysql.*", "")
    setup_util.replace_text("nodejs/package.json", ".*mapper.*", "")
  
  try:
    subprocess.check_call("npm install", shell=True, cwd="nodejs")
  finally:
    if os.name == 'nt':
      subprocess.check_call("del package.json", shell=True, cwd="nodejs")
      subprocess.check_call("ren package.json.dist package.json", shell=True, cwd="nodejs")

def stop():
  if os.name == 'nt':
    subprocess.Popen("taskkill /f /im node.exe > NUL", shell=True)
    return 0
  
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
