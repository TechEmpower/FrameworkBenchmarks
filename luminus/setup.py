
import subprocess
import sys
import setup_util

def start(args, logfile):
  setup_util.replace_text("luminus/hello/src/hello/models/schema.clj", ":subname \"//.*:3306", ":subname \"//" + args.database_host + ":3306")

  try:
    subprocess.check_call("lein ring uberwar", shell=True, cwd="luminus/hello")
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
    subprocess.check_call("cp luminus/hello/target/hello-luminus-standalone.war $RESIN_HOME/webapps/luminus.war", shell=True)
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
