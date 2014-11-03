import subprocess
import sys
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args, logfile, errfile):
  setup_util.replace_text("nawak/model_redis.nim",
                          'open\(host=.*\)',
                          'open(host="' + args.database_host + '")')
  # compile the app
  subprocess.check_call(
      "nimrod c --threads:on -d:release -d:redis_model --path:../installs/nawak/nawak -o:nawak_redis app.nim",
      shell=True, cwd="nawak", stderr=errfile, stdout=logfile)
  # launch mongrel2
  subprocess.check_call("mkdir -p run logs tmp", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  subprocess.check_call("sudo m2sh load -config mongrel2.conf", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  subprocess.check_call("sudo m2sh start -name test", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  
  # launch workers
  subprocess.Popen("./nawak_redis", shell=True, cwd="nawak", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  ret = 0

  try:
    subprocess.check_call("sudo m2sh stop -every", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  except Exception:
    ret = 1

  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'nawak_redis' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)
      except OSError:
        ret = 1

  return ret
