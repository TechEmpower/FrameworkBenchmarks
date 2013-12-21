import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("ninja-standalone/src/main/resources/conf/application.conf", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")
  
  try:
    subprocess.check_call("mvn clean compile assembly:single", shell=True, cwd="ninja-standalone", stderr=errfile, stdout=logfile)
    subprocess.check_call("java -Dninja.port=8080 -jar target/hello-ninja-standalone-0.0.1-SNAPSHOT-jar-with-dependencies.jar", cwd="ninja-standalone", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'hello-ninja' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)
  return 0
