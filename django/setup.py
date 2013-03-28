import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("django/hello/hello/settings.py", "HOST': '.*'", "HOST': '" + args.database_host + "'")
  # for the love of all that is good and right in the universe, use gevent worker
  # if you're going to bother to use gunicorn.  meinheld is faster, but needs some work on 
  # certain asynch support.
  # $ easy_install pip
  # $ pip install gevent
  # not so difficult, is it?
  # requires 5 minutes of searching
  # so much for CTO Outsourcing
  subprocess.Popen("gunicorn hello.wsgi:application -k gevent  -b 0.0.0.0:8080 -w " + str((args.max_threads * 2)) + " --log-level=critical", shell=True, cwd="django/hello")
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
