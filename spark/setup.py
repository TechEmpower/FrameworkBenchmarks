
import subprocess
import sys
import setup_util

def start(args, logfile):
  setup_util.replace_text("spark/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")
  
  try:
    subprocess.check_call("mvn clean package", shell=True, cwd="spark", stderr=logfile, stdout=logfile)
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("cp spark/target/spark.war $RESIN_HOME/webapps/spark.war", shell=True, stderr=logfile, stdout=logfile)
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