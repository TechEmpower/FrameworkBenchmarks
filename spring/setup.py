import subprocess
import sys
import setup_util
import os

def start(args):
  try:
    subprocess.check_call("mvn clean package", shell=True, cwd="spring")
    subprocess.Popen("java -Ddatabase.host=" + args.database_host + " -jar spring.jar".rsplit(" "), cwd="spring/target")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  if os.name == 'nt':
    subprocess.check_call("wmic process where \"CommandLine LIKE '%spring%'\" call terminate")
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'spring' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
  return 0