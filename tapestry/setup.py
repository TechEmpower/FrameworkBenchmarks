
import subprocess
import sys
import setup_util

def start(args, logfile):
  setup_util.replace_text("tapestry/hello/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

  try:
    subprocess.check_call("mvn clean compile war:war", shell=True, cwd="tapestry/hello")
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
    subprocess.check_call("cp tapestry/hello/target/tapestry.war $RESIN_HOME/webapps/tapestry.war", shell=True)
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