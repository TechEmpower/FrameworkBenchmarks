import subprocess
import sys
#import setup_util
import json
from os.path import expanduser
import os
import getpass

home = expanduser("~")

def start(args, logfile, errfile):
  # setup_util.replace_text("mojolicious/app.pl", "localhost", ""+ args.database_host +"")
  # str(args.max_threads)
  conf = { 
    'database_host': args.database_host,
    'workers': args.max_threads,
  }
  with open('mojolicious/app.conf', 'w') as f:
    f.write(json.dumps(conf))

  try:
    # os.environ["MOJO_MODE"] = "production"
    subprocess.Popen("hypnotoad ./app.pl", shell=True, cwd="mojolicious", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  try:
    subprocess.call("hypnotoad -s ./app.pl", shell=True, cwd="mojolicious", stderr=errfile, stdout=logfile)
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'hypnotoad' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)
    return 0
  except subprocess.CalledProcessError:
    return 1
