
import subprocess
import sys
import setup_util
import os
import glob

def start(args, logfile):
  setup_util.replace_text("scalatra/src/main/webapp/WEB-INF/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

  try:
    if os.name == 'nt':
      subprocess.check_call('"..\\sbt\\sbt.bat" clean package', shell=True, cwd="scalatra")
      subprocess.check_call('rmdir /S /Q "%RESIN_HOME%\\webapps\\"', shell=True)
      subprocess.check_call('mkdir "%RESIN_HOME%\\webapps\\"', shell=True)
      
      # 'copy' on windows doesn't like the wildcard war file selector
      warFile = glob.glob("scalatra\\target\\scala-2.10\\scalatra*.war")[0]
      subprocess.check_call('copy "{0}" "%RESIN_HOME%\\webapps\\scalatra.war"'.format(warFile), shell=True)
      subprocess.check_call('"%RESIN_HOME%\\bin\\start.bat"', shell=True)
    else:
      subprocess.check_call("../sbt/sbt clean package", shell=True, cwd="scalatra")
      subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
      subprocess.check_call("cp scalatra/target/scala-2.10/scalatra*.war $RESIN_HOME/webapps/scalatra.war", shell=True)
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
