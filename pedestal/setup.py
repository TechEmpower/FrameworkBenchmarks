
import subprocess
import sys
import setup_util

def start(args):
  setup_util.replace_text("pedestal/helloworld/project.clj", ":subname \"//.*:3306", ":subname \"//" + args.database_host + ":3306")

  try:
    subprocess.check_call("lein ring uberwar", shell=True, cwd="pedestal/helloworld")
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
    subprocess.check_call("cp pedestal/helloworld/target/helloworld-pedestal-standalone.war $RESIN_HOME/webapps/pedestal.war", shell=True)
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
