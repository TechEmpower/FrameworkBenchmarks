import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("php-phpixie/assets/config/db.php", "localhost", "" + args.database_host + "")
  setup_util.replace_text("php-phpixie/deploy/php-phpixie", "\".*\/FrameworkBenchmarks/php-phpixie", "\"" + args.troot)
  setup_util.replace_text("php-phpixie/deploy/php-phpixie", "Directory .*\/FrameworkBenchmarks/php-phpixie", "Directory " + args.troot)
  setup_util.replace_text("php-phpixie/deploy/nginx.conf", "root .*\/FrameworkBenchmarks/php-phpixie", "root " + args.troot)

  try:
    subprocess.check_call("composer.phar install --optimize-autoloader", shell=True, cwd="php-phpixie", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo $PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/deploy/nginx.conf", shell=True, stderr=errfile, stdout=logfile)
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
