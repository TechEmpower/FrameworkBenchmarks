import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("snap/bench/cfg/db.cfg", "host=\".*\"", "host=\"" + args.database_host + "\"")
  subprocess.check_call("cabal update", shell=True, cwd="snap/bench", stderr=errfile, stdout=logfile)
  subprocess.check_call("cabal install --only-dependencies", shell=True, cwd="snap/bench", stderr=errfile, stdout=logfile)
  subprocess.check_call("cabal configure", shell=True, cwd="snap/bench", stderr=errfile, stdout=logfile)
  subprocess.check_call("cabal build", shell=True, cwd="snap/bench", stderr=errfile, stdout=logfile)

  subprocess.Popen("dist/build/snap-bench/snap-bench +RTS -A4M -N -qg2 -I0 -G2", shell=True, cwd="snap/bench", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'snap-bench' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)
      except OSError:
        pass

  return 0
