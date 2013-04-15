
import subprocess
import sys
import setup_util

def start(args):
  setup_util.replace_text("compojure/hello/src/hello/handler.clj", ":subname \"//.*:3306", ":subname \"//" + args.database_host + ":3306")

  try:
    subprocess.check_call("lein ring uberwar", shell=True, cwd="compojure/hello")
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
    subprocess.check_call("cp compojure/hello/target/hello-compojure-standalone.war $RESIN_HOME/webapps/compojure.war", shell=True)
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
