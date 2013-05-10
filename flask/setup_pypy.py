import subprocess
import sys
import setup_util
import os

proc = None

def start(args):
  global proc
  setup_util.replace_text("flask/app.py", "DBHOSTNAME", args.database_host)
  proc = subprocess.Popen("~/FrameworkBenchmarks/installs/pypy-2.0/bin/pypy run_pypy.py --port=8080 --logging=error", shell=True, cwd="flask")
  return 0

def stop():
  global proc
  if not proc:
    return 0
  proc.terminate()
  ret = proc.wait()
  proc = None
  return ret
