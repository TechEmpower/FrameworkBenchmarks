import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text('undertow/src/main/resources/hello/server.properties', 'DATABASE_HOST', args.database_host)
  try:
    subprocess.check_call("mvn clean compile assembly:single", shell=True, cwd="undertow", stderr=errfile, stdout=logfile)
    subprocess.Popen("java -jar undertow-example-0.1-jar-with-dependencies.jar".rsplit(" "), cwd="undertow/target", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'undertow-example' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0