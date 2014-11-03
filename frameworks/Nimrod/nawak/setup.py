import subprocess
import sys
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args, logfile, errfile):
  setup_util.replace_text("nawak/model_postgre.nim", "host=.* port=5432",
                          "host=" + args.database_host + " port=5432")
  # compile the app
  subprocess.check_call(
      "nim c --threads:on -d:release -d:postgre_model --path:$NAWAK_PATH -o:nawak_postgre app.nim",
      shell=True, cwd="nawak", stderr=errfile, stdout=logfile)
  # launch mongrel2
  subprocess.check_call("mkdir -p run logs tmp", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  subprocess.check_call("sudo m2sh load -config mongrel2.conf", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  subprocess.check_call("sudo m2sh start -name test", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  
  # launch workers
  subprocess.Popen("./nawak_postgre", shell=True, cwd="nawak", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  ret = 0

  try:
    subprocess.check_call("sudo m2sh stop -every", shell=True, cwd="nawak/conf", stderr=errfile, stdout=logfile)
  except:
    ret = 1

  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'nawak_postgre' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)
      except OSError:
        ret = 1

  return ret
