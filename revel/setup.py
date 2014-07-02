import subprocess
import sys
import os
import setup_util
import time



def start(args, logfile, errfile):
  setup_util.replace_text("revel/src/benchmark/conf/app.conf", "tcp\(.*:3306\)", "tcp(" + args.database_host + ":3306)")
  if os.name == 'nt':
    env = os.environ.copy()
    env["GOPATH"] = r"C:\FrameworkBenchmarks\revel"
    subprocess.call("go get -u github.com/robfig/revel/revel", shell=True, cwd="revel", env=env, stderr=errfile, stdout=logfile)
    subprocess.call(r"go build -o bin\revel.exe github.com/robfig/revel/revel", shell=True, cwd="revel", env=env, stderr=errfile, stdout=logfile)
    subprocess.Popen(r"bin\revel.exe run benchmark prod".rsplit(" "), shell=True, cwd="revel", env=env, stderr=errfile, stdout=logfile)
    return 0
  os.environ["GOPATH"] = os.path.expanduser('~/FrameworkBenchmarks/revel')
  subprocess.call("go get -u github.com/robfig/revel/revel", shell=True, cwd="revel", stderr=errfile, stdout=logfile)
  subprocess.call("go build -o bin/revel github.com/robfig/revel/revel", shell=True, cwd="revel", stderr=errfile, stdout=logfile)
  subprocess.Popen("bin/revel run benchmark prod".rsplit(" "), cwd="revel", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.call("taskkill /f /im benchmark.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("taskkill /f /im revel.exe > NUL", shell=True, stderr=errfile, stdout=logfile)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'revel' in line and 'run-tests' not in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
