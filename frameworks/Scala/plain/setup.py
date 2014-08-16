import setup_util
import subprocess
import sys
import time
import os

def start(args, logfile, errfile):
  setup_util.replace_text("plain/src/main/resources/application.conf", "127.0.0.1", args.database_host)
  if os.name == 'nt':
    subprocess.check_call(".\sbt.bat assembly && del /f /s /q target\scala-2.10\cache", shell=True, cwd="plain", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("./sbt assembly && rm -rf target/scala-2.10/cache", shell=True, cwd="plain", stderr=errfile, stdout=logfile)

  subprocess.Popen("java -jar target/scala-2.10/plain-benchmark-assembly-1.0.1.jar", cwd="plain", shell=True, stderr=errfile, stdout=logfile)
  time.sleep(10)
  return 0

def stop(logfile, errfile):
  if os.name == 'nt':
    subprocess.call("taskkill /f /im *plain-benchmark* > NUL", shell=True, stderr=errfile, stdout=logfile)
  else:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'plain-benchmark' in line:
        try:
          pid = int(line.split(None, 2)[1])
          os.kill(pid, 15)
        except OSError:
          return 1

  # Takes up so much disk space
  if os.name == 'nt':
    subprocess.check_call("del /f /s /q target && del /f /s /q project", shell=True, cwd="plain", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("rm -rf target && rm -rf project/.ivy && rm -rf project/.boot", shell=True, cwd="plain", stderr=errfile, stdout=logfile)
  
  return 0
