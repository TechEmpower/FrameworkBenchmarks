import subprocess
import sys
import os
import setup_util
import time

def start(args):
  setup_util.replace_text("revel/src/benchmark/conf/app.conf", "tcp\(.*:3306\)", "tcp(" + args.database_host + ":3306)")
  if os.name == 'nt':
    subprocess.Popen("setup.bat", shell=True, cwd="revel")
    return 0
  subprocess.call("go get github.com/robfig/revel/cmd", shell=True, cwd="revel")
  subprocess.call("go build -o bin/revel github.com/robfig/revel/cmd", shell=True, cwd="revel")
  subprocess.Popen("bin/revel run benchmark prod".rsplit(" "), cwd="revel")
  return 0

def stop():
  if os.name == 'nt':
    subprocess.call("taskkill /f /im go.exe > NUL", shell=True)
    subprocess.call("taskkill /f /im revel.exe > NUL", shell=True)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'revel' in line and 'run-tests' not in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
