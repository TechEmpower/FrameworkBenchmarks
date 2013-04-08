
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("yesod/bench/config/mysql.yml", "host: .*", "host: " + args.database_host)
  
  subprocess.check_call("cabal update", shell=True, cwd="yesod/bench")
  subprocess.check_call("cabal install --only-dependencies", shell=True, cwd="yesod/bench")
  subprocess.check_call("cabal build", shell=True, cwd="yesod/bench")

  subprocess.Popen("dist/build/bench/bench Production +RTS -A4M -N -qg2 -I0 -G2 > /dev/null", shell=True, cwd="yesod/bench")
  return 0

def stop():
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
