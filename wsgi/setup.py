
import subprocess
import sys
import setup_util
import os

def start(args):
  subprocess.Popen('gunicorn hello:app --worker-class="egg:meinheld#gunicorn_worker" -b 0.0.0.0:8080 -w '
                   + str((args.max_threads * 2)) + " --log-level=critical", shell=True, cwd="wsgi")
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
