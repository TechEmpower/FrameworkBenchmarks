
import subprocess
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("hhvm/once.php.inc", "host=localhost;", "host=" + args.database_host + ";")
  setup_util.replace_text("hhvm/deploy/config.hdf", "SourceRoot = .*\/FrameworkBenchmarks/hhvm", "SourceRoot = " + args.troot)
  setup_util.replace_text("hhvm/deploy/config.hdf", "Path = .*\/.hhvm.hhbc", "Path = " + args.troot + "/.hhvm.bbhc")
  setup_util.replace_text("hhvm/deploy/config.hdf", "PidFile = .*\/hhvm.pid", "PidFile = " + args.troot + "/hhvm.pid")
  setup_util.replace_text("hhvm/deploy/config.hdf", "File = .*\/error.log", "File = " + args.troot + "/error.log")

  try:
    if os.name == 'nt':
      # Not supported !
      return 0
    subprocess.check_call("hhvm --config $TROOT/deploy/config.hdf -m daemon", shell=True, stderr=errfile, stdout=logfile)
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
      if 'hhvm' in line and 'toolset' not in line and 'run-ci' not in line and 'run-tests' not in line:
        pid = int(line.split(None,2)[1])
        os.kill(pid,15)
    return 0
  except subprocess.CalledProcessError:
    return 1
