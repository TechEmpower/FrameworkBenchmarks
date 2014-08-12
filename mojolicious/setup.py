import subprocess
import sys
import json
import os
import getpass

def start(args, logfile, errfile):
  conf = { 
    'database_host' : args.database_host,
    'workers'       : args.max_threads,
  }
  with open('mojolicious/app.conf', 'w') as f:
    f.write(json.dumps(conf))

  try:
    # os.environ["MOJO_MODE"] = "production"
    subprocess.Popen("hypnotoad $TROOT/app.pl", shell=True, cwd="mojolicious", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  try:
    subprocess.call("hypnotoad -s $TROOT/app.pl", shell=True, cwd="mojolicious", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

