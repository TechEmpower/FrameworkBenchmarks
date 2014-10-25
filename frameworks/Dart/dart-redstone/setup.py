import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text('dart-redstone/postgresql.yaml', 'host: .*', 'host: ' + args.database_host)
  setup_util.replace_text('dart-redstone/mongodb.yaml', 'host: .*', 'host: ' + args.database_host)
  try:
    #
    # install dart dependencies
    #
    subprocess.check_call('pub upgrade', shell=True, cwd='dart-redstone', stderr=errfile, stdout=logfile)
    #
    # start dart servers
    #
    subprocess.Popen('dart server.dart -a 0.0.0.0 -p 8080 -d ' + str(args.max_concurrency) + ' -i ' + str(args.max_threads), shell=True, cwd='dart-redstone', stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  #
  # stop dart servers
  #
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'dart' in line and 'run-tests' not in line and 'run-ci' not in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
