
import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("restexpress/config/dev/environment.properties", "mongodb:\/\/.*\/hello_world", "mongodb://" + args.database_host + "/hello_world")
  setup_util.replace_text("restexpress/config/dev/environment.properties", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

  try:
    subprocess.check_call("mvn clean package", shell=True, cwd="restexpress", stderr=errfile, stdout=logfile)
    subprocess.check_call("mvn assembly:single", shell=True, cwd="restexpress", stderr=errfile, stdout=logfile)
    subprocess.check_call("unzip world-1.0-SNAPSHOT-zip-with-dependencies.zip", shell=True, cwd="restexpress/target", stderr=errfile, stdout=logfile)
    subprocess.Popen("java -jar world-1.0-SNAPSHOT.jar".rsplit(" "), cwd="restexpress/target/world-1.0-SNAPSHOT", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'world-1.0-SNAPSHOT.jar' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0