
import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("unfiltered/src/main/resources/application.conf", "jdbc:mysql:\/\/.*:3306", "jdbc:mysql://" + args.database_host + ":3306")
  setup_util.replace_text("unfiltered/src/main/resources/application.conf", "maxThreads = \\d+", "maxThreads = " + str(args.max_threads))

  subprocess.check_call("chmod u+x sbt", shell=True, cwd="unfiltered")
  subprocess.check_call("./sbt assembly", shell=True, cwd="unfiltered")
  subprocess.Popen("java -jar bench-assembly-1.0.0.jar", shell=True, cwd="unfiltered/target/scala-2.10")

  return 0
def stop():
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if './start' in line or 'java' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 9)

  return 0
