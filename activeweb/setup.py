
import subprocess
import sys
import os
import setup_util

def start(args, logfile, errfile):
  test = args
  bm = test.benchmarker

  setup_util.replace_text("activeweb/src/main/webapp/WEB-INF/resin-web.xml", "localhost", bm.database_host)
  setup_util.replace_text("activeweb/src/main/java/app/config/DbConfig.java", "localhost", bm.database_host)

  try:
    test.run("mvn clean  package", cwd="activeweb")
    if os.name == 'nt':
      test.run("rmdir /s /q C:\\Java\\resin\\webapps")
      test.run("mkdir C:\\Java\\resin\\webapps")
      test.run("cp activeweb\\target\\activeweb.war C:\\Java\\resin\\webapps\\activeweb.war")
      test.run("C:\\Java\\resin\\bin\\start.bat")
      return 0
    test.run("rm -rf $RESIN_HOME/webapps/*")
    test.run("cp activeweb/target/activeweb.war $RESIN_HOME/webapps/")
    test.run("$RESIN_HOME/bin/resinctl start")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    if os.name == 'nt':
      test.run("C:\\Java\\resin\\bin\\stop.bat")
      return 0
    test.run("$RESIN_HOME/bin/resinctl shutdown")
    return 0
  except subprocess.CalledProcessError:
    return 1
