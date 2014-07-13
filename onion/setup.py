import subprocess
import sys
import os
import setup_util 

def start(args, logfile, errfile):
  setup_util.replace_text("onion/hello.c", "mysql_real_connect\(data.db\[i\], \".*\",", "mysql_real_connect(data.db[i], \"" + args.database_host + "\",")
  os.putenv("ONION_LOG","noinfo")
  subprocess.call("rm *.o", cwd="onion", shell=True, stderr=errfile, stdout=logfile)
  subprocess.call("cp -R installs/onion/* onion/onion", shell=True, stderr=errfile, stdout=logfile)
  subprocess.call("rm CMakeCache.txt", shell=True, cwd="onion/onion/build", stderr=errfile, stdout=logfile)
  subprocess.Popen("make && ./hello", shell=True, cwd="onion", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
