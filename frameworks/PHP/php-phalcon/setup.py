import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("php-phalcon/app/config/config.php", "mongodb:\/\/localhost", "mongodb://" + args.database_host)
  setup_util.replace_text("php-phalcon/app/config/config.php", "localhost", ""+ args.database_host +"")
  setup_util.replace_text("php-phalcon/deploy/nginx.conf", "root .*\/FrameworkBenchmarks/php-phalcon", "root " + args.troot)

  try:
    subprocess.check_call("sudo chown -R www-data:www-data php-phalcon", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo $PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/deploy/nginx.conf", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("sudo kill -QUIT $( cat $TROOT/deploy/php-fpm.pid )", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo chown -R $USER:$USER php-phalcon", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
