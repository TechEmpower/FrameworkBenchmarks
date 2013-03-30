
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("django/hello/hello/settings.py", "HOST': '.*'", "HOST': '" + args.database_host + "'")
  
  subprocess.check_call("cabal configure", shell=True, cwd="yesod/src")
  subprocess.check_call("cabal build", shell=True, cwd="yesod/src")

  subprocess.Popen("./dist/yesod_bench Production +RTS -N" + str(args.max_threads ) + " > /dev/null", shell=True, cwd="yesod/src")
  return 0
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'yesod_bench' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
      except OSError:
        pass

  return 0