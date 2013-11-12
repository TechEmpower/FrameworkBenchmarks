import subprocess
import sys
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args, logfile):
  setup_util.replace_text("php-kohana/application/config/database.php", "localhost", ""+ args.database_host +"")
  setup_util.replace_text("php-kohana/deploy/nginx.conf", "root .*\/FrameworkBenchmarks", "root " + home + "/FrameworkBenchmarks")

  try:
    if os.name == 'nt':
      subprocess.check_call('icacls "C:\\FrameworkBenchmarks\\php-kohana" /grant "IIS_IUSRS:(OI)(CI)F"', shell=True)
      subprocess.check_call('appcmd add site /name:PHP /bindings:http/*:8080: /physicalPath:"C:\\FrameworkBenchmarks\\php-kohana"', shell=True)
      return 0
    subprocess.check_call("sudo chown -R www-data:www-data php-kohana", shell=True)
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf -g " + home + "/FrameworkBenchmarks/php-kohana/deploy/php-fpm.pid", shell=True)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/php-kohana/deploy/nginx.conf", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  try:
    if os.name == 'nt':
      subprocess.check_call('appcmd delete site PHP', shell=True)
      return 0
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    subprocess.call("sudo kill -QUIT $( cat php-kohana/deploy/php-fpm.pid )", shell=True)
    subprocess.check_call("sudo chown -R $USER:$USER php-kohana", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1