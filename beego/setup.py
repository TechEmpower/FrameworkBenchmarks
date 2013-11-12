
import subprocess
import sys
import os

def start(args, logfile):
  if os.name == 'nt':
    subprocess.call("set GOPATH=C:\\FrameworkBenchmarks\\beego&&go get ./...", shell=True, cwd="beego", stderr=logfile, stdout=logfile)
    subprocess.Popen("setup.bat", shell=True, cwd="beego", stderr=logfile, stdout=logfile)
    return 0
  subprocess.call("go get ./...", shell=True, cwd="beego", stderr=logfile, stdout=logfile)
  subprocess.Popen("go run src/hello/hello.go".rsplit(" "), cwd="beego", stderr=logfile, stdout=logfile)
  return 0
def stop(logfile):
  if os.name == 'nt':
    subprocess.call("taskkill /f /im go.exe > NUL", shell=True, stderr=logfile, stdout=logfile)
    subprocess.call("taskkill /f /im hello.exe > NUL", shell=True, stderr=logfile, stdout=logfile)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
