
import subprocess
import sys
import os

def start(args, logfile):
  if os.name == 'nt':
    subprocess.call("set GOPATH=C:\\FrameworkBenchmarks\\webgo&&go get ./...", shell=True, cwd="webgo")
    subprocess.Popen("setup.bat", shell=True, cwd="webgo") 
    return 0
  subprocess.call("go get ./...", shell=True, cwd="webgo")
  subprocess.Popen("go run src/hello/hello.go".rsplit(" "), cwd="webgo")
  return 0
def stop(logfile):
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
