import subprocess
import sys
import os
import setup_util
import time

CWD = 'revel-qbs'

def start(args):
  setup_util.replace_text(CWD + "/src/benchmark/conf/app.conf", "tcp\(.*:3306\)", "tcp(" + args.database_host + ":3306)")
  if os.name == 'nt':
    env = os.environ.copy()
    env["GOPATH"] = "C:\\FrameworkBenchmarks\\" + CWD
    subprocess.call("go get -u github.com/robfig/revel/revel github.com/coocood/qbs", shell=True, cwd=CWD, env=env)
    subprocess.call(r"go build -o bin\revel.exe github.com/robfig/revel/revel", shell=True, cwd=CWD, env=env)
    subprocess.Popen(r"bin\revel.exe run benchmark prod".rsplit(" "), shell=True, cwd=CWD, env=env)
    return 0
  subprocess.call("go get -u github.com/robfig/revel/revel github.com/coocood/qbs", shell=True, cwd=CWD)
  subprocess.call("go build -o bin/revel github.com/robfig/revel/revel", shell=True, cwd=CWD)
  subprocess.Popen("bin/revel run benchmark prod".rsplit(" "), cwd=CWD)
  return 0

def stop():
  if os.name == 'nt':
    subprocess.call("taskkill /f /im benchmark.exe > NUL", shell=True)
    subprocess.call("taskkill /f /im revel.exe > NUL", shell=True)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'revel' in line and 'run-tests' not in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
