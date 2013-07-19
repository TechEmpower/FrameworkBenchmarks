import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("bottle/app.py", "DBHOSTNAME", args.database_host)
  subprocess.Popen("gunicorn app:app --worker-class=meinheld.gmeinheld.MeinheldWorker -b 0.0.0.0:8080 -w " +
                   str((args.max_threads * 2)) + " --preload --log-level=critical", shell=True, cwd="bottle")
  return 0

def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'gunicorn' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
      except OSError:
        pass
  
  return 0
