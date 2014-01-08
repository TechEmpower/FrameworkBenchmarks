import subprocess
import sys
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args, logfile, errfile):
  # compile the app
  setup_util.replace_text("nawak/nawak_app.nim", "host=.* port=5432", "host=" + args.database_host + " port=5432")
  subprocess.check_call("nimrod c -d:release --path:../installs/nawak/nawak nawak_app.nim",
                        shell=True, cwd="nawak", stderr=errfile, stdout=logfile)
  # launch mongrel2
  subprocess.check_call("mkdir -p run logs tmp", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  subprocess.check_call("m2sh load -config mongrel2.conf", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  subprocess.check_call("sudo m2sh start -name test", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  
  for i in range(0, 2):
    # launch workers
    subprocess.Popen("./nawak_app " + str(i), shell=True, cwd="nawak", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  subprocess.check_call("sudo m2sh stop -every", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)

  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'nawak_app' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
      except OSError:
        pass

  return 0
