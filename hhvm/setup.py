
import subprocess
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args, logfile, errfile):
  setup_util.replace_text("hhvm/once.php.inc", "host=.*;", "host=" + args.database_host + ";")
  setup_util.replace_text("hhvm/deploy/config.hdf", "SourceRoot = .*\/FrameworkBenchmarks", "SourceRoot = " + home + "/FrameworkBenchmarks")
  setup_util.replace_text("hhvm/deploy/config.hdf", "Path = .*\/.hhvm.hhbc", "Path = " + home + "/FrameworkBenchmarks/hhvm/.hhvm.bbhc")
  setup_util.replace_text("hhvm/deploy/config.hdf", "PidFile = .*\/hhvm.pid", "Path = " + home + "/FrameworkBenchmarks/hhvm/hhvm.pid")

  try:
    if os.name == 'nt':
      # Not supported !
      return 0
    subprocess.check_call("sudo hhvm --config " + home + "/FrameworkBenchmarks/hhvm/deploy/config.hdf -m daemon", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    if os.name == 'nt':
      # Not Supported !
      return 0
    subprocess.call("sudo kill -QUIT $( cat hhvm/hhvm.pid )", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
