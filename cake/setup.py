
import subprocess
import sys
import os
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args, logfile):
  setup_util.replace_text("cake/app/Config/database.php", "'host' => '.*',", "'host' => '" + args.database_host + "',")
  setup_util.replace_text("cake/deploy/cake", "\".*\/FrameworkBenchmarks", "\"" + home + "/FrameworkBenchmarks")
  setup_util.replace_text("cake/deploy/cake", "Directory .*\/FrameworkBenchmarks", "Directory " + home + "/FrameworkBenchmarks")
  setup_util.replace_text("cake/deploy/nginx.conf", "root .*\/FrameworkBenchmarks", "root " + home + "/FrameworkBenchmarks")

  try:
    if os.name == 'nt':
      setup_util.replace_text("cake/app/Config/core.php", "'Apc'", "'Wincache'")
      subprocess.check_call('icacls "C:\\FrameworkBenchmarks\\cake" /grant "IIS_IUSRS:(OI)(CI)F"', shell=True)
      subprocess.check_call('appcmd add site /name:PHP /bindings:http/*:8080: /physicalPath:"C:\\FrameworkBenchmarks\\cake\\app\\webroot"', shell=True)
      return 0
    #subprocess.check_call("sudo cp cake/deploy/cake /etc/apache2/sites-available/", shell=True)
    #subprocess.check_call("sudo a2ensite cake", shell=True)
    subprocess.check_call("sudo chown -R www-data:www-data cake", shell=True)
    #subprocess.check_call("sudo /etc/init.d/apache2 start", shell=True)
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf -g " + home + "/FrameworkBenchmarks/cake/deploy/php-fpm.pid", shell=True)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/cake/deploy/nginx.conf", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  try:
    if os.name == 'nt':
      subprocess.call('appcmd delete site PHP', shell=True)
      return 0
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    subprocess.call("sudo kill -QUIT $( cat cake/deploy/php-fpm.pid )", shell=True)
    #subprocess.check_call("sudo a2dissite cake", shell=True)
    #subprocess.check_call("sudo /etc/init.d/apache2 stop", shell=True)
    subprocess.check_call("sudo chown -R $USER:$USER cake", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
