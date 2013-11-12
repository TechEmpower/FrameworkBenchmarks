import subprocess
import sys
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args, logfile):
  subprocess.check_call("nimrod c -d:release --path:../installs/jester/jester hello.nim", shell=True, cwd="jester", stderr=logfile, stdout=logfile)
  subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/jester/config/nginx.conf", shell=True, stderr=logfile, stdout=logfile)
  
  for i in range(0, 8):
    subprocess.Popen("./hello 900" + str(i), shell=True, cwd="jester", stderr=logfile, stdout=logfile)
  return 0

def stop(logfile):
  subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=logfile, stdout=logfile)

  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
      except OSError:
        pass

  return 0
