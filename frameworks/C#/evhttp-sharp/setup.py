import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  if os.name == 'nt':
    return 1
  
  # build
  subprocess.check_call("rm -rf bin obj", shell=True, cwd="evhttp-sharp", stdout=logfile, stderr=errfile)
  subprocess.check_call("xbuild /p:Configuration=Release", shell=True, cwd="evhttp-sharp/src", stdout=logfile, stderr=errfile)
  os.environ['MONO_GC_PARAMS']="nursery-size=64m"
  subprocess.Popen("mono -O=all $TROOT/src/bin/Release/EvHttpSharpBenchmark.exe 127.0.0.1 8085 " + str(args.max_threads), shell=True, cwd="evhttp-sharp", stdout=logfile, stderr=errfile)

def stop(logfile, errfile):
  if os.name == 'nt':
    return 0
  
  # stop mono
  p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
  out, err = p.communicate()
  for line in out.splitlines():
    if 'mono' in line and not 'run-ci' in line and not 'run-tests' in line:
      pid = int(line.split(None, 2)[1])
      os.kill(pid, 15)

  return 0
