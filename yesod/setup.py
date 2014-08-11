
import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  #setup_util.replace_text("yesod/bench/config/mysql.yml", "host: .*", "host: " + args.database_host)
  
  subprocess.check_call("cabal update", shell=True, cwd="yesod/bench", stderr=errfile, stdout=logfile)
  subprocess.check_call("cabal install", shell=True, cwd="yesod/bench", stderr=errfile, stdout=logfile)
  subprocess.check_call("cabal configure", shell=True, cwd="yesod/bench", stderr=errfile, stdout=logfile)
  subprocess.check_call("cabal build", shell=True, cwd="yesod/bench", stderr=errfile, stdout=logfile)

  db_host = args.database_host
  threads = str(args.max_threads)
  subprocess.Popen("dist/build/bench/bench " + threads + " " + db_host + " +RTS -A32m -qa -N", shell=True, cwd="yesod/bench", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'bench' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)
      except OSError:
        pass

  return 0
