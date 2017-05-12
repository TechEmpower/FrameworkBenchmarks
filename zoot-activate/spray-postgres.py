import subprocess
import sys
import setup_util
import os
import time

def start(args, logfile, errfile):

  env = os.environ.copy()
  env["activate.storage.benchmark.factory"] = "net.fwbrasil.activate.storage.relational.async.AsyncPostgreSQLStorageFactory"
  env["activate.storage.benchmark.host"] = args.database_host
  env["activate.storage.benchmark.database"] = "hello_world"
  env["activate.storage.benchmark.user"] = "benchmarkdbuser"
  env["activate.storage.benchmark.password"] = "benchmarkdbpass"
  env["activate.storage.benchmark.poolMaxQueueSize"] = "10000"
  env["activate.storage.benchmark.poolMaxObjects"] = "1700"
  env["activate.storage.benchmark.poolMaxIdle"] = "99999999"
  env["server"] = "spray"

  if os.name == 'nt':
    subprocess.Popen('"..\\sbt\\sbt.bat" clean run', shell=True, cwd="zoot-activate", stderr=errfile, stdout=logfile, env=env)
  else:
    subprocess.Popen("../sbt/sbt clean run", shell=True, cwd="zoot-activate", stderr=errfile, stdout=logfile, env=env)

  time.sleep(60)
  return 0

def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%server.Main%'\" call terminate", stderr=errfile, stdout=logfile)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'zoot' in line and 'java' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)
        
  return 0
