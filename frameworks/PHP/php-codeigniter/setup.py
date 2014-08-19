import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("php-codeigniter/application/config/database.php", "localhost", ""+ args.database_host +"")
  setup_util.replace_text("php-codeigniter/deploy/nginx.conf", "root .*\/FrameworkBenchmarks/php-codeigniter", "root " + args.troot)

  try:
    if os.name == 'nt':
      subprocess.check_call('icacls "C:\\FrameworkBenchmarks\\php-codeigniter" /grant "IIS_IUSRS:(OI)(CI)F"', shell=True, stderr=errfile, stdout=logfile)
      subprocess.check_call('appcmd add site /name:PHP /bindings:http/*:8080: /physicalPath:"C:\\FrameworkBenchmarks\\php-codeigniter"', shell=True, stderr=errfile, stdout=logfile)
      return 0
    subprocess.check_call("sudo chown -R www-data:www-data php-codeigniter", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo $PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/deploy/nginx.conf", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    if os.name == 'nt':
      subprocess.call('appcmd delete site PHP', shell=True, stderr=errfile, stdout=logfile)
      return 0
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("sudo kill -QUIT $( cat $TROOT/deploy/php-fpm.pid )", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo chown -R $USER:$USER php-codeigniter", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1