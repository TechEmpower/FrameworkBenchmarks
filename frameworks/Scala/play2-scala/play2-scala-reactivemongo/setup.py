
import subprocess
import sys
import setup_util
import os
from zipfile import ZipFile

def start(args, logfile, errfile):
  setup_util.replace_text("play2-scala-reactivemongo/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")

  subprocess.check_call("play clean dist", shell=True, cwd="play2-scala-reactivemongo", stderr=errfile, stdout=logfile)

  if os.name == 'nt':
    ZipFile("./play2-scala-reactivemongo/target/universal/play2-scala-reactivemongo-1.0-SNAPSHOT.zip").extractall("./play2-scala-reactivemongo/target/universal")
    with open("./play2-scala-reactivemongo/target/universal/play2-scala-reactivemongo-1.0-SNAPSHOT/bin/play2-scala-reactivemongo.bat", "w+") as f:
      f.write("java %1 -cp \"./lib/*;\" play.core.server.NettyServer .")
    subprocess.Popen("play2-scala-reactivemongo.bat", shell=True, cwd="play2-scala-reactivemongo/target/universal/play2-scala-reactivemongo-1.0-SNAPSHOT/bin", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("unzip play2-scala-reactivemongo-1.0-SNAPSHOT.zip", shell=True, cwd="play2-scala-reactivemongo/target/universal", stderr=errfile, stdout=logfile)
    subprocess.check_call("chmod +x play2-scala-reactivemongo", shell=True, cwd="play2-scala-reactivemongo/target/universal/play2-scala-reactivemongo-1.0-SNAPSHOT/bin", stderr=errfile, stdout=logfile)
    subprocess.Popen("./play2-scala-reactivemongo", shell=True, cwd="play2-scala-reactivemongo/target/universal/play2-scala-reactivemongo-1.0-SNAPSHOT/bin", stderr=errfile, stdout=logfile)

  return 0
def stop(logfile, errfile):
  if os.name == 'nt':
    with open("./play2-scala-reactivemongo/target/universal/play2-scala-reactivemongo-1.0-SNAPSHOT/RUNNING_PID") as f:
      pid = int(f.read())
      os.kill(pid, 15)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'NettyServer' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)

  try:
    os.remove("play2-scala-reactivemongo/target/universal/play2-scala-reactivemongo-1.0-SNAPSHOT/RUNNING_PID")
  except OSError:
    return 1

  # Takes up so much disk space
  if os.name == 'nt':
    subprocess.check_call("del /f /s /q target", shell=True, cwd="play2-scala-reactivemongo", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("rm -rf target", shell=True, cwd="play2-scala-reactivemongo", stderr=errfile, stdout=logfile)

  return 0
