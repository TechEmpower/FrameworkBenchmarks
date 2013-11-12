
import subprocess
import sys
import os
import setup_util

def start(args, logfile):
  setup_util.replace_text("servlet/src/main/webapp/WEB-INF/resin-web.xml", "localhost", args.database_host)

  try:
    subprocess.check_call("mvn clean compile war:war", shell=True, cwd="servlet", stderr=logfile, stdout=logfile)
    if os.name == 'nt':
      subprocess.check_call("rmdir /s /q C:\\Java\\resin\\webapps", shell=True, stderr=logfile, stdout=logfile)
      subprocess.check_call("mkdir C:\\Java\\resin\\webapps", shell=True, stderr=logfile, stdout=logfile)
      subprocess.check_call("cp servlet\\target\\servlet.war C:\\Java\\resin\\webapps\\servlet.war", shell=True, stderr=logfile, stdout=logfile)
      subprocess.check_call("C:\\Java\\resin\\bin\\start.bat", shell=True, stderr=logfile, stdout=logfile)
      return 0
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("cp servlet/target/servlet.war $RESIN_HOME/webapps/", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True, stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  try:
    if os.name == 'nt':
      subprocess.check_call("C:\\Java\\resin\\bin\\stop.bat", shell=True, stderr=logfile, stdout=logfile)
      return 0
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
