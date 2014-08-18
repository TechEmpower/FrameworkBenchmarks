import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("php-silex/web/index_raw.php", "192.168.100.102", "" + args.database_host + "")
  setup_util.replace_text("php-silex/deploy/php-silex", "\".*\/FrameworkBenchmarks/php-silex", "\"" + args.troot)
  setup_util.replace_text("php-silex/deploy/php-silex", "Directory .*\/FrameworkBenchmarks/php-silex", "Directory " + args.troot)
  setup_util.replace_text("php-silex/deploy/nginx.conf", "root .*\/FrameworkBenchmarks/php-silex", "root " + args.troot)

  try:
    subprocess.check_call("composer.phar install --optimize-autoloader", shell=True, cwd="php-silex", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo $PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/deploy/nginx_raw.conf", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("sudo kill -QUIT $( cat $TROOT/deploy/php-fpm.pid )", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
