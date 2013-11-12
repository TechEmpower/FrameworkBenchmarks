import subprocess
import sys
import setup_util
import os

def start(args, logfile):
  setup_util.replace_text("spring/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")
  
  try:
    subprocess.check_call("mvn clean compile war:war", shell=True, cwd="spring")

    if os.name == 'nt':
      subprocess.check_call('rmdir /S /Q "%RESIN_HOME%\\webapps\\"', shell=True)
      subprocess.check_call('mkdir "%RESIN_HOME%\\webapps\\"', shell=True)
      subprocess.check_call('copy spring\\target\\spring.war "%RESIN_HOME%\\webapps\\spring.war"', shell=True)
      subprocess.check_call('"%RESIN_HOME%\\bin\\start.bat"', shell=True)
    else:
      subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
      subprocess.check_call("cp spring/target/spring.war $RESIN_HOME/webapps/spring.war", shell=True)
      subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  try:
    if os.name == 'nt':
      subprocess.check_call('"%RESIN_HOME%\\bin\\stop.bat"', shell=True)
    else:
      subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
