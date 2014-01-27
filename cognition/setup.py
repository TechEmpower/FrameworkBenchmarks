
import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  max_connections = str(256 / args.max_threads)
  host_name = "jdbc:mysql://" + args.database_host + ":3306"
  setup_util.replace_text("cognition/src/main/resources/application.conf", "jdbc:mysql:\/\/.*:3306", host_name)
  setup_util.replace_text("cognition/project/Build.scala", "jdbc:mysql:\/\/.*:3306", host_name)
  setup_util.replace_text("cognition/src/main/resources/application.conf", "maxConnections = \\d+", "maxConnections = " + max_connections)
  setup_util.replace_text("cognition/src/main/resources/application.conf", "minConnections = \\d+", "minConnections = " + max_connections)
  setup_util.replace_text("cognition/src/main/resources/application.conf", "partitionCount = \\d+", "partitionCount = " + str(args.max_threads) )
  subprocess.check_call("../sbt/sbt clean compile stage", shell=True, cwd="cognition", stderr=errfile, stdout=logfile)
  subprocess.Popen("./bench", shell=True, cwd="cognition/target/universal/stage/bin", stderr=errfile, stdout=logfile)

  return 0
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'bench' in line or 'java' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)

  # Takes up so much disk space
  if os.name == 'nt':
    subprocess.check_call("del /f /s /q target", shell=True, cwd="cognition", stderr=errfile, stdout=logfile)
    subprocess.check_call("del /f /s /q target", shell=True, cwd="cognition/project", stderr=errfile, stdout=logfile)
  else:
    subprocess.check_call("rm -rf target", shell=True, cwd="cognition", stderr=errfile, stdout=logfile)
    subprocess.check_call("rm -rf target", shell=True, cwd="cognition/project", stderr=errfile, stdout=logfile)

  return 0
