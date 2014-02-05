import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  try:
    subprocess.check_call("mvn clean compile assembly:single", shell=True, cwd="grizzly-bm", stderr=errfile, stdout=logfile)
    subprocess.Popen("java -Dorg.glassfish.grizzly.nio.transport.TCPNIOTransport.max-receive-buffer-size=16384 -Dorg.glassfish.grizzly.http.io.OutputBuffer.default-buffer-size=1024 -Dorg.glassfish.grizzly.memory.BuffersBuffer.bb-cache-size=32 -jar grizzly-bm-0.1-jar-with-dependencies.jar".rsplit(" "), cwd="grizzly-bm/target", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'grizzly-bm' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)
  return 0