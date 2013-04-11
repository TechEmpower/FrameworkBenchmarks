import subprocess
import sys
import os
import setup_util 

def start(args):
  setup_util.replace_text("onion/hello.c", "mysql_real_connect\(data.db\[i\], \".*\",", "mysql_real_connect(data.db[i], \"" + args.database_host + "\",")
  os.putenv("ONION_LOG","noinfo")
  subprocess.Popen("make && ./hello", shell=True, cwd="onion")
  return 0

def stop():
  
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
