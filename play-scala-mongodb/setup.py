
import subprocess
import sys
import setup_util
import os
from zipfile import ZipFile

def start(args, logfile):
  setup_util.replace_text("play-scala-mongodb/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")

  subprocess.check_call("play clean dist", shell=True, cwd="play-scala-mongodb", stderr=logfile, stdout=logfile)

  if os.name == 'nt':
    ZipFile("./play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT.zip").extractall("./play-scala-mongodb/target/universal")
    with open("./play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/bin/play-scala-mongodb.bat", "w+") as f:
      f.write("java %1 -cp \"./lib/*;\" play.core.server.NettyServer .")
    subprocess.Popen("play-scala-mongodb.bat", shell=True, cwd="play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/bin", stderr=logfile, stdout=logfile)
  else:
    subprocess.check_call("unzip play-scala-mongodb-1.0-SNAPSHOT.zip", shell=True, cwd="play-scala-mongodb/target/universal", stderr=logfile, stdout=logfile)
    subprocess.check_call("chmod +x play-scala-mongodb", shell=True, cwd="play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/bin", stderr=logfile, stdout=logfile)
    subprocess.Popen("./play-scala-mongodb", shell=True, cwd="play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/bin", stderr=logfile, stdout=logfile)

  return 0
def stop(logfile):
  if os.name == 'nt':
    with open("./play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/RUNNING_PID") as f:
      pid = int(f.read())
      os.kill(pid, 9)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'NettyServer' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)

  try:
    os.remove("play-scala-mongodb/target/universal/play-scala-mongodb-1.0-SNAPSHOT/RUNNING_PID")
  except OSError:
    pass

  return 0
