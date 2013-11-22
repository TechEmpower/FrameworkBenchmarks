
import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("grails/hello/grails-app/conf/DataSource.groovy", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  
  try:
    subprocess.check_call("grails -non-interactive -plain-output compile", shell=True, cwd="grails/hello", stderr=errfile, stdout=logfile)
    subprocess.check_call("grails prod -non-interactive -plain-output war", shell=True, cwd="grails/hello", stderr=errfile, stdout=logfile)
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("cp grails/hello/target/hello-0.1.war $RESIN_HOME/webapps/grails.war", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("rm -rf $RESIN_HOME/resin-data/*", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
