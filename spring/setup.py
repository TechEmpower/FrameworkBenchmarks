
import subprocess
import sys
import setup_util

def start(args):
  setup_util.replace_text("spring/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")
  
  try:
    subprocess.check_call("mvn clean compile war:war", shell=True, cwd="spring")
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
    subprocess.check_call("cp spring/target/spring.war $RESIN_HOME/webapps/spring.war", shell=True)
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