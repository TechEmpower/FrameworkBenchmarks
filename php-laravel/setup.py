import subprocess
import sys
import os
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args, logfile):
  setup_util.replace_text("php-laravel/application/config/database.php", "localhost", ""+ args.database_host +"")
  setup_util.replace_text("php-laravel/deploy/nginx.conf", "root .*\/FrameworkBenchmarks", "root " + home + "/FrameworkBenchmarks")

  try:
    if os.name == 'nt':
      subprocess.check_call('icacls "C:\\FrameworkBenchmarks\\php-laravel" /grant "IIS_IUSRS:(OI)(CI)F"', shell=True, stderr=logfile, stdout=logfile)
      subprocess.check_call('appcmd add site /name:PHP /bindings:http/*:8080: /physicalPath:"C:\\FrameworkBenchmarks\\php-laravel\\public"', shell=True, stderr=logfile, stdout=logfile)
      return 0
    subprocess.check_call("sudo chown -R www-data:www-data php-laravel", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf -g " + home + "/FrameworkBenchmarks/php-laravel/deploy/php-fpm.pid", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/php-laravel/deploy/nginx.conf", shell=True, stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  try:
    if os.name == 'nt':
      subprocess.call('appcmd delete site PHP', shell=True, stderr=logfile, stdout=logfile)
      return 0
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=logfile, stdout=logfile)
    subprocess.call("sudo kill -QUIT $( cat php-laravel/deploy/php-fpm.pid )", shell=True, stderr=logfile, stdout=logfile)
    subprocess.check_call("sudo chown -R $USER:$USER php-laravel", shell=True, stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1