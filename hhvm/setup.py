
import subprocess
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args):
  if not args.database_host:
    args.database_host = "localhost"

  setup_util.replace_text("hhvm/db.php", "@.*\/hello_world", "@" + args.database_host + "/hello_world")
  setup_util.replace_text("hhvm/queries.php", "@.*\/hello_world", "@" + args.database_host + "/hello_world")
  setup_util.replace_text("hhvm/fortunes.php", "@.*\/hello_world", "@" + args.database_host + "/hello_world")
  setup_util.replace_text("hhvm/updates.php", "@.*\/hello_world", "@" + args.database_host + "/hello_world")

  setup_util.replace_text("hhvm/deploy/config.hdf", "SourceRoot = .*\/FrameworkBenchmarks", "SourceRoot = " + home + "/FrameworkBenchmarks")
  setup_util.replace_text("hhvm/deploy/config.hdf", "Path = .*\/.hhvm.hhbc", "Path = " + home + "/FrameworkBenchmarks/hhvm/.hhvm.bbhc")
  setup_util.replace_text("hhvm/deploy/config.hdf", "PidFile = .*\/hhvm.pid", "Path = " + home + "/FrameworkBenchmarks/hhvm/hhvm.pid")

  try:
    if os.name == 'nt':
      # Not supported !
      return 1
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
