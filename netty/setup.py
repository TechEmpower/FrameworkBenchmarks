import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  try:
    subprocess.check_call("mvn clean compile assembly:single", shell=True, cwd="netty", stderr=errfile, stdout=logfile)
    subprocess.Popen("java -Dio.netty.noResourceLeakDetection=true -jar netty-example-0.1-jar-with-dependencies.jar".rsplit(" "), cwd="netty/target", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%netty-example%'\" call terminate", stderr=errfile, stdout=logfile)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'netty-example' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
  return 0