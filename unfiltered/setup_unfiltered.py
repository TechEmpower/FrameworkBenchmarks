
import subprocess
import sys
import setup_util
import os

def start(args, logfile):
  setup_util.replace_text("unfiltered/src/main/resources/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  setup_util.replace_text("unfiltered/src/main/resources/application.conf", "maxThreads = \\d+", "maxThreads = " + str(args.max_threads))

  # Shamelessly stolen from stack overflow
  try:
    from subprocess import DEVNULL
  except ImportError:
    import os
    DEVNULL = open(os.devnull, 'wb')

  subprocess.check_call("../sbt/sbt assembly", shell=True, cwd="unfiltered", stderr=logfile, stdout=logfile)
  subprocess.Popen("java -jar bench-assembly-1.0.0.jar", stderr=DEVNULL, shell=True, cwd="unfiltered/target/scala-2.10", stderr=logfile, stdout=logfile)

  return 0
def stop(logfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'bench-assembly' in line or 'java' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)

  return 0
