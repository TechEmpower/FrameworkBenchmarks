
import subprocess
import sys
import setup_util
import os
from zipfile import ZipFile

def start(args, logfile, errfile):
  setup_util.replace_text("play-activate-mysql/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")

  subprocess.check_call("play clean dist", shell=True, cwd="play-activate-mysql", stderr=errfile, stdout=logfile)

  if os.name == 'nt':
    ZipFile("./play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT.zip").extractall("./play-activate-mysql/target/universal")
    with open("./play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/bin/play-activate-mysql.bat", "w+") as f:
      f.write("java %1 -cp \"./lib/*;\" play.core.server.NettyServer .")
    subprocess.Popen("play-activate-mysql.bat", shell=True, cwd="play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/bin", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("unzip play-activate-mysql-1.0-SNAPSHOT.zip", shell=True, cwd="play-activate-mysql/target/universal", stderr=errfile, stdout=logfile)
    subprocess.check_call("chmod +x play-activate-mysql", shell=True, cwd="play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/bin", stderr=errfile, stdout=logfile)
    subprocess.Popen("./play-activate-mysql", shell=True, cwd="play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/bin", stderr=errfile, stdout=logfile)

  return 0
def stop(logfile, errfile):
  try:
    with open("./play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/RUNNING_PID") as f:
      pid = int(f.read())
      os.kill(pid,15)
  except Exception:
    return 1

  try:
    os.remove("play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/RUNNING_PID")
  except OSError:
    return 1

  # Takes up so much disk space
  if os.name == 'nt':
    subprocess.check_call("del /f /s /q target", shell=True, cwd="play-activate-mysql", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("rm -rf target", shell=True, cwd="play-activate-mysql", stderr=errfile, stdout=logfile)

  return 0