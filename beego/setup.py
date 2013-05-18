
import subprocess
import sys
import os

def start(args):
  if os.name == 'nt':
    subprocess.call("set GOPATH=C:\\FrameworkBenchmarks\\beego&&go get ./...", shell=True, cwd="beego")
    subprocess.Popen("setup.bat", shell=True, cwd="beego")
    return 0
  subprocess.call("go get ./...", shell=True, cwd="beego")
  subprocess.Popen("go run src/hello/hello.go".rsplit(" "), cwd="beego")
  return 0
def stop():
  if os.name == 'nt':
    subprocess.call("taskkill /f /im go.exe > NUL", shell=True)
    subprocess.call("taskkill /f /im hello.exe > NUL", shell=True)
    return 0
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
