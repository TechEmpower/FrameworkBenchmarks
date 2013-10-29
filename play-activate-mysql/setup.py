
import subprocess
import sys
import setup_util
import os
from zipfile import ZipFile

def start(args):
  setup_util.replace_text("play-activate-mysql/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")

  subprocess.check_call("play clean dist", shell=True, cwd="play-activate-mysql")

  if os.name == 'nt':
    ZipFile("./play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT.zip").extractall("./play-activate-mysql/target/universal")
    with open("./play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/bin/play-activate-mysql.bat", "w+") as f:
      f.write("java %1 -cp \"./lib/*;\" play.core.server.NettyServer .")
    subprocess.Popen("play-activate-mysql.bat", shell=True, cwd="play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/bin")
  else:
    subprocess.check_call("unzip play-activate-mysql-1.0-SNAPSHOT.zip", shell=True, cwd="play-activate-mysql/target/universal")
    subprocess.check_call("chmod +x play-activate-mysql", shell=True, cwd="play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/bin")
    subprocess.Popen("./play-activate-mysql", shell=True, cwd="play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/bin")

  return 0
def stop():
  if os.name == 'nt':
    with open("./play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/RUNNING_PID") as f:
      pid = int(f.read())
      os.kill(pid, 9)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'NettyServer' in line or ('netty' in line and 'java' in line):
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)

  try:
    os.remove("play-activate-mysql/target/universal/play-activate-mysql-1.0-SNAPSHOT/RUNNING_PID")
  except OSError:
    pass

  return 0
