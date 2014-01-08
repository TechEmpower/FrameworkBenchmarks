import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("ninja-resin/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")
  
  try:
    subprocess.check_call("mvn clean compile war:war", shell=True, cwd="ninja-resin", stderr=errfile, stdout=logfile)

    if os.name == 'nt':
      subprocess.check_call('rmdir /S /Q "%RESIN_HOME%\\webapps\\"', shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call('mkdir "%RESIN_HOME%\\webapps\\"', shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call('copy ninja-resin\\target\\ninja-resin.war "%RESIN_HOME%\\webapps\\ninja-resin.war"', shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call('"%RESIN_HOME%\\bin\\start.bat"', shell=True, stderr=errfile, stdout=logfile)
    else:
      subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call("cp ninja-resin/target/ninja-resin.war $RESIN_HOME/webapps/ninja-resin.war", shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call("$RESIN_HOME/bin/resinctl", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    if os.name == 'nt':
      subprocess.check_call('"%RESIN_HOME%\\bin\\stop.bat"', shell=True, stderr=errfile, stdout=logfile)
    else:
      subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
