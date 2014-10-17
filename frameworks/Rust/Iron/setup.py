import subprocess
import sys
import os
import setup_util 

def start(args, logfile, errfile):
  setup_util.replace_text("Iron/src/main.rs", "127.0.0.1", args.database_host)
  subprocess.check_call("cargo build --release", shell=True, cwd="Iron", stderr=errfile, stdout=logfile)
  subprocess.Popen("cargo run -v --release", shell=True, cwd="Iron", stderr=errfile, stdout=logfile)
  return 0

def stop(logfile, errfile):
  
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'cargo' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
