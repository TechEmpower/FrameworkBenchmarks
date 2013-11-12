import subprocess
import sys
import setup_util
import os

def start(args, logfile):
  if os.name == 'nt':
    return 1
  
  setup_util.replace_text("aspnet/src/Web.config", "localhost", args.database_host)

  try:
    subprocess.check_call("rm -rf bin obj", shell=True, cwd="aspnet/src")
    subprocess.check_call("xbuild /p:Configuration=Release", shell=True, cwd="aspnet/src", stderr=logfile, stdout=logfile)
    subprocess.check_call("sudo chown -R ubuntu:ubuntu /usr/local/etc/mono", shell=True)
    subprocess.Popen("MONO_OPTIONS=--gc=sgen xsp4 --nonstop", shell=True, cwd="aspnet/src", stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile):
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