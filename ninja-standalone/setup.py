import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("ninja/src/main/resource/conf/application.conf", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")
  
  try:
    subprocess.check_call("mvn clean compile assembly:single", shell=True, cwd="ninja")
    subprocess.check_call("java -Dninja.port=8080 -Dninja.mode=prod -Dninja.context=/ninja -jar ninja/target/hello-ninja-standalone-0.0.1-SNAPSHOT-jar-with-dependencies.jar", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello-ninja' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
