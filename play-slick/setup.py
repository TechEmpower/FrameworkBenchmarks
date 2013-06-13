
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("play-slick/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")

  subprocess.check_call("play dist", shell=True, cwd="play-slick")
  subprocess.check_call("unzip play-slick-1.0-SNAPSHOT.zip", shell=True, cwd="play-slick/dist")
  subprocess.check_call("chmod +x start", shell=True, cwd="play-slick/dist/play-slick-1.0-SNAPSHOT")
  subprocess.Popen("./start", shell=True, cwd="play-slick/dist/play-slick-1.0-SNAPSHOT")

  return 0
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if './start' in line or ('play' in line and 'java' in line):
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  try:
    os.remove("play-slick/RUNNING_PID")
  except OSError:
    pass

  return 0
