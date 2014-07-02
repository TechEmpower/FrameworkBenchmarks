
import subprocess
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args, logfile, errfile):
  setup_util.replace_text("hhvm/once.php.inc", "host=localhost;", "host=" + args.database_host + ";")
  setup_util.replace_text("hhvm/deploy/config.hdf", "SourceRoot = .*\/FrameworkBenchmarks", "SourceRoot = " + home + "/FrameworkBenchmarks")
  setup_util.replace_text("hhvm/deploy/config.hdf", "Path = .*\/.hhvm.hhbc", "Path = " + home + "/FrameworkBenchmarks/hhvm/.hhvm.bbhc")
  setup_util.replace_text("hhvm/deploy/config.hdf", "PidFile = .*\/hhvm.pid", "PidFile = " + home + "/FrameworkBenchmarks/hhvm/hhvm.pid")
  setup_util.replace_text("hhvm/deploy/config.hdf", "File = .*\/error.log", "File = " + home + "/FrameworkBenchmarks/hhvm/error.log")


  try:
    if os.name == 'nt':
      # Not supported !
      return 0
    subprocess.check_call("hhvm --config " + home + "/FrameworkBenchmarks/hhvm/deploy/config.hdf -m daemon", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    if os.name == 'nt':
      # Not Supported !
      return 0
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'hhvm' in line and 'toolset' not in line:
        pid = int(line.split(None,2)[1])
        os.kill(pid,15)
    return 0
  except subprocess.CalledProcessError:
    return 1
