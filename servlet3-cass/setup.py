
import subprocess
import sys
import os
import setup_util

def start(args, logfile, errfile):

  try:
    subprocess.check_call("mvn clean compile war:war", shell=True, cwd="servlet3-cass", stderr=errfile, stdout=logfile)
    if os.name == 'nt':
      subprocess.check_call("rmdir /s /q C:\\Java\\resin\\webapps", shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call("mkdir C:\\Java\\resin\\webapps", shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call("cp servlet3-cass\\target\\servlet3-cass.war C:\\Java\\resin\\webapps\\servlet3-cass.war", shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call("C:\\Java\\resin\\bin\\start.bat", shell=True, stderr=errfile, stdout=logfile)
      return 0
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("cp servlet/target/servlet3-cass.war $RESIN_HOME/webapps/", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    if os.name == 'nt':
      subprocess.check_call("C:\\Java\\resin\\bin\\stop.bat", shell=True, stderr=errfile, stdout=logfile)
      return 0
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
