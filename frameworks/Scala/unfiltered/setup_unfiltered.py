
import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("unfiltered/src/main/resources/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  setup_util.replace_text("unfiltered/src/main/resources/application.conf", "maxThreads = \\d+", "maxThreads = " + str(args.max_threads))

  subprocess.check_call("../sbt/sbt assembly", shell=True, cwd="unfiltered", stderr=errfile, stdout=logfile)
  subprocess.Popen("java -jar bench-assembly-1.0.0.jar", shell=True, cwd="unfiltered/target/scala-2.10", stderr=errfile, stdout=logfile)

  return 0
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'bench-assembly' in line or 'java' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)

  # Takes up so much disk space
  if os.name == 'nt':
    subprocess.check_call("del /f /s /q target", shell=True, cwd="unfiltered", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("rm -rf target", shell=True, cwd="unfiltered", stderr=errfile, stdout=logfile)

  return 0
