
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("play1siena/conf/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  
  subprocess.check_call("play war -o ../play1siena --zip", shell=True, cwd="play1siena")
  # TODO deploy to resin
  #subprocess.check_call("unzip play-java-1.0-SNAPSHOT.zip", shell=True, cwd="play-java/dist")
  #subprocess.check_call("chmod +x start", shell=True, cwd="play-java/dist/play-java-1.0-SNAPSHOT")
  # TODO start resin
  # subprocess.Popen("./start", shell=True, cwd="play-java/dist/play-java-1.0-SNAPSHOT")

  return 0
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if './start' in line or ('play' in line and 'java' in line):   
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  try:
    os.remove("play-java/RUNNING_PID")
  except OSError:
    pass
    
  return 0
