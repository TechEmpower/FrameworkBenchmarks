
import subprocess
import sys
import setup_util
import os

def start(args, logfile):
  setup_util.replace_text("play1siena/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  
  subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=logfile, stdout=logfile)
  subprocess.check_call("play1 war -o $RESIN_HOME/webapps/play1 --exclude benchmark_config", shell=True, cwd="play1siena", stderr=logfile, stdout=logfile)
  subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True, stderr=logfile, stdout=logfile)

  return 0
def stop(logfile):
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
