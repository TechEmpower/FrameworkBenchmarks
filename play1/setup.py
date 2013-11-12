import subprocess
import sys
import setup_util

def start(args, logfile):
  setup_util.replace_text("play1/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  subprocess.check_call("play1 start --%prod", shell=True, cwd="play1", stderr=logfile, stdout=logfile)
#  subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
#  subprocess.check_call("play1 war -o $RESIN_HOME/webapps/play1 --exclude benchmark_config", shell=True, cwd="play1")
#  subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True)
  return 0
def stop(logfile):
  try:
    subprocess.check_call("play1 stop", shell=True, cwd="play1", stderr=logfile, stdout=logfile)
#    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True)
#    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1 
