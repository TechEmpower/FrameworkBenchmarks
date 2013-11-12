
import subprocess
import sys
import setup_util

def start(args, logfile):
  setup_util.replace_text("wicket/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

  try:
    subprocess.check_call("mvn clean compile war:war", shell=True, cwd="wicket", stderr=logfile, stdout=logfile)
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("cp wicket/target/hellowicket-1.0-SNAPSHOT.war $RESIN_HOME/webapps/wicket.war", shell=True, stderr=logfile, stdout=logfile)
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