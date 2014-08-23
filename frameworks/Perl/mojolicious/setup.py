import subprocess
import json
import os
import multiprocessing

def start(args, logfile, errfile):
  conf = { 
    'database_host' : args.database_host,
    'hypnotoad_merge' : {
      'workers' : 2*multiprocessing.cpu_count(), 
      # can use args.max_threads and args.max_concurrency to set
    },
  }
  with open(args.troot + '/app.conf', 'w') as f:
    f.write(json.dumps(conf))

  try:
    # os.environ["MOJO_MODE"] = "production"
    subprocess.Popen("carton exec hypnotoad $TROOT/app.pl", shell=True, cwd="mojolicious", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  try:
    subprocess.call("carton exec hypnotoad -s $TROOT/app.pl", shell=True, cwd="mojolicious", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

