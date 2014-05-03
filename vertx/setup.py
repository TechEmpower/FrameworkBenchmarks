import multiprocessing
import subprocess
import sys
import setup_util
import os

nCpu = multiprocessing.cpu_count()

def start(args, logfile, errfile):
  setup_util.replace_text("vertx/app.js", "host: '.*'", "host: '" + args.database_host + "'")
  try:
    subprocess.Popen("vertx run app.js", shell=True, cwd="vertx", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'app.js' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)

  return 0
