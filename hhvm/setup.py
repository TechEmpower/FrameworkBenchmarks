
import subprocess
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args):
  if not args.database_host:
    args.database_host = "localhost"
  setup_util.replace_text("hhvm/dborm.php", "@.*\/hello_world", "@" + args.database_host + "/hello_world")
  setup_util.replace_text("hhvm/dbraw.php", "host=.*;", "host=" + args.database_host + ";")
  setup_util.replace_text("hhvm/updateraw.php", "host=.*;", "host=" + args.database_host + ";")
  setup_util.replace_text("hhvm/fortune.php", "host=.*;dbname", "host=" + args.database_host + ";dbname")
  setup_util.replace_text("hhvm/deploy/config.hdf", "SourceRoot = .*\/FrameworkBenchmarks", "SourceRoot = " + home + "/FrameworkBenchmarks")
  setup_util.replace_text("hhvm/deploy/config.hdf", "File = .*\/hhvm-error.log", "File = " + home + "/FrameworkBenchmarks/hhvm/hhvm-error.log")
  setup_util.replace_text("hhvm/deploy/config.hdf", "File = .*\/hhvm-access.log", "File = " + home + "/FrameworkBenchmarks/hhvm/hhvm-access.log")
  setup_util.replace_text("hhvm/deploy/config.hdf", "Path = .*\/.hhvm.hhbc", "Path = " + home + "/FrameworkBenchmarks/hhvm/.hhvm.bbhc")
  setup_util.replace_text("hhvm/deploy/config.hdf", "PidFile = .*\/hhvm.pid", "Path = " + home + "/FrameworkBenchmarks/hhvm/hhvm.pid")

  try:
    if os.name == 'nt':
        # Not supported !
        #subprocess.check_call('appcmd add site /name:PHP /bindings:http/*:8080: /physicalPath:"C:\\FrameworkBenchmarks\\php"', shell=True)
      return 0
    subprocess.check_call("sudo hhvm --config " + home + "/FrameworkBenchmarks/hhvm/deploy/config.hdf -m daemon", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    if os.name == 'nt':
      subprocess.check_call('appcmd delete site PHP', shell=True)
      return 0

    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    subprocess.call("sudo kill -QUIT $( cat php/deploy/php-fpm.pid )", shell=True)

    return 0
  except subprocess.CalledProcessError:
    return 1
