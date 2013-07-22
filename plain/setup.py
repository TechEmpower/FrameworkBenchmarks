
import subprocess
import sys
import time
import os

def start(args=None):
    subprocess.check_call("./sbt assembly", shell=True, cwd="plain")
    subprocess.Popen("java -server -Xnoclassgc -XX:MaxPermSize=1g -XX:ReservedCodeCacheSize=384m -Xmx8g -Xss8m -Xmn4g -jar target/scala-2.10/plain-benchmark-assembly-1.0.1.jar", cwd="plain", shell=True)
    time.sleep(10)
    return 0

def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'plain-benchmark' in line:
      try:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
      except OSError:
        pass
  
  return 0
