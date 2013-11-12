
import subprocess
import sys
import setup_util

def start(args, logfile):
  setup_util.replace_text("compojure/hello/src/hello/handler.clj", ":subname \"//.*:3306", ":subname \"//" + args.database_host + ":3306")

  try:
    subprocess.check_call("lein ring uberwar", shell=True, cwd="compojure/hello", stderr=logfile, stdout=logfile)
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("cp compojure/hello/target/hello-compojure-standalone.war $RESIN_HOME/webapps/compojure.war", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True, stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
