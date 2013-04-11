
import subprocess
import sys
import os

def start(args):
  subprocess.call("go get ./...", shell=True, cwd="webgo")
  subprocess.Popen("go run src/hello/hello.go".rsplit(" "), cwd="webgo")
  return 0
def stop():
  
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
