import subprocess
import sys
import os

def start(args, logfile, errfile):
  try:
    subprocess.check_call("mvn clean compile assembly:single", shell=True, cwd="jetty-servlet", stderr=errfile, stdout=logfile)
    subprocess.Popen("java -jar jetty-servlet-example-0.1-jar-with-dependencies.jar".rsplit(" "), cwd="jetty-servlet/target", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'jetty-servlet-example' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0
