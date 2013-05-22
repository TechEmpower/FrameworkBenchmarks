import subprocess
import sys
import setup_util
import os

def start(args):
  if os.name == 'nt':
    return 1
  
  setup_util.replace_text("servicestack/src/Web.config", "localhost", args.database_host)

  try:
    subprocess.check_call("rm -rf bin obj", shell=True, cwd="aspnet/src")
    subprocess.check_call("xbuild /p:Configuration=Release", shell=True, cwd="servicestack/src/Web.config")
    subprocess.Popen("xsp4 --nonstop", shell=True, cwd="servicestack/src/Web.config")
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  if os.name == 'nt':
    return 0
  
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'xsp4' in line:
      pid = int(line.split(None, 2)[1])
      try:
        os.kill(pid, 9)
      except OSError:
        pass
  return 0