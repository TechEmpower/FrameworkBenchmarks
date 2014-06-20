import subprocess
import sys
import setup_util
import os

root = os.getcwd() + "/evhttp-sharp"
app = root + "/src"

def start(args, logfile, errfile):
  if os.name == 'nt':
    return 1

  try:
    # build
    subprocess.check_call("rm -rf bin obj", shell=True, cwd=app, stdout=logfile, stderr=errfile)
    subprocess.check_call("xbuild /p:Configuration=Release", shell=True, cwd=app, stdout=logfile, stderr=errfile)
    os.environ['MONO_GC_PARAMS']="nursery-size=64m"
    subprocess.Popen("mono -O=all bin/Release/EvHttpSharpBenchmark.exe 127.0.0.1 8085 " + str(args.max_threads) + " &", shell=True, cwd=app, stdout=logfile, stderr=errfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  if os.name == 'nt':
    return 0
  
  subprocess.check_call("pkill -9 mono", shell=True)
  return 0
