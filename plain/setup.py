import setup_util
import subprocess
import sys
import time
import os

def start(args):
  setup_util.replace_text("plain/src/main/resources/application.conf", "127.0.0.1", args.database_host)
  if os.name == 'nt':
    subprocess.check_call(".\sbt.bat assembly && del /f /s /q target\scala-2.10\cache", shell=True, cwd="plain")
  else:
    subprocess.check_call("./sbt assembly && rm -rf target/scala-2.10/cache", shell=True, cwd="plain")

  subprocess.Popen("java -server -da -dsa -Xrs -Xmx6g -Xmn4g -Xss8m -Xnoclassgc -XX:MaxPermSize=1g -XX:ReservedCodeCacheSize=384m -XX:+UseG1GC -XX:MaxGCPauseMillis=200 -jar target/scala-2.10/plain-benchmark-assembly-1.0.1.jar", cwd="plain", shell=True)
  time.sleep(10)
  return 0

def stop():
  if os.name == 'nt':
    subprocess.call("taskkill /f /im *plain-benchmark* > NUL", shell=True)
    return 0

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
