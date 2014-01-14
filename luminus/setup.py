
import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("luminus/hello/src/hello/models/schema.clj", ":subname \"//.*:3306", ":subname \"//" + args.database_host + ":3306")

  try:
    subprocess.check_call("lein clean", shell=True, cwd="compojure/hello", stderr=errfile, stdout=logfile)
    subprocess.check_call("lein ring uberwar", shell=True, cwd="luminus/hello" stderr=errfile, stdout=logfile)
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
    subprocess.check_call("cp luminus/hello/target/hello-luminus-standalone.war $RESIN_HOME/webapps/luminus.war", shell=True)
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
