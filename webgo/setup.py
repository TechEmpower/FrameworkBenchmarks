
import subprocess
import sys
import os

def start(args):
  
  subprocess.Popen("go run hello.go".rsplit(" "), cwd="webgo")
  return 0
def stop():
  
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'a.out' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0