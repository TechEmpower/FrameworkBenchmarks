import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("php-pimf/app/config.app.php", "127.0.0.1", "" + args.database_host + "")
  setup_util.replace_text("php-pimf/deploy/php-pimf", "\".*\/FrameworkBenchmarks/php-pimf", "\"" + args.troot)
  setup_util.replace_text("php-pimf/deploy/php-pimf", "Directory .*\/FrameworkBenchmarks/php-pimf", "Directory " + args.troot)
  setup_util.replace_text("php-pimf/deploy/nginx_raw.conf", "root .*\/FrameworkBenchmarks/php-pimf", "root " + args.troot)

  try:
    subprocess.check_call("composer.phar install --optimize-autoloader", shell=True, cwd="php-pimf")
    subprocess.check_call("sudo $PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid", shell=True)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/deploy/nginx_raw.conf", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    subprocess.call("sudo kill -QUIT $( cat $TROOT/deploy/php-fpm.pid )", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
