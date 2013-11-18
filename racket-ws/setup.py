
import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  db_host = args.database_host
  threads = str(args.max_threads)
  subprocess.Popen("racket -t bench.rkt -- " + db_host + " > /dev/null", shell=True, cwd="racket-ws/bench", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'bench' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
      except OSError:
        pass

  return 0
