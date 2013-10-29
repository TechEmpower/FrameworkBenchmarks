
import subprocess
import sys
import setup_util
import os
from zipfile import ZipFile

def start(args):
  setup_util.replace_text("play-scala-mongodb/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")

  subprocess.check_call("play clean dist", shell=True, cwd="play-scala-mongodb")

  if os.name == 'nt':
    ZipFile("./play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT.zip").extractall("./play-scala-mongodb/target/universal")
    with open("./play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/bin/play-scala-mongodb.bat", "w+") as f:
      f.write("java %1 -cp \"./lib/*;\" play.core.server.NettyServer .")
    subprocess.Popen("play-scala-mongodb.bat", shell=True, cwd="play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/bin")
  else:
    subprocess.check_call("unzip play-scala-mongodb-1.0-SNAPSHOT.zip", shell=True, cwd="play-scala-mongodb/target/universal")
    subprocess.check_call("chmod +x play-scala-mongodb", shell=True, cwd="play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/bin")
    subprocess.Popen("./play-scala-mongodb", shell=True, cwd="play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/bin")

  return 0
def stop():
  if os.name == 'nt':
    with open("./play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/RUNNING_PID") as f:
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
    os.remove("play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/RUNNING_PID")
  except OSError:
    pass

  return 0
