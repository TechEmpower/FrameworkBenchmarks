import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args, logfile, errfile):
  setup_util.replace_text("php-phpixie/assets/config/db.php", "localhost", "" + args.database_host + "")
  setup_util.replace_text("php-phpixie/deploy/php-phpixie", "\".*\/FrameworkBenchmarks", "\"" + home + "/FrameworkBenchmarks")
  setup_util.replace_text("php-phpixie/deploy/php-phpixie", "Directory .*\/FrameworkBenchmarks", "Directory " + home + "/FrameworkBenchmarks")
  setup_util.replace_text("php-phpixie/deploy/nginx.conf", "root .*\/FrameworkBenchmarks", "root " + home + "/FrameworkBenchmarks")

  try:
    subprocess.check_call("composer.phar install --optimize-autoloader", shell=True, cwd="php-phpixie", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf -g " + home + "/FrameworkBenchmarks/php-phpixie/deploy/php-fpm.pid", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/php-phpixie/deploy/nginx.conf", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("sudo kill -QUIT $( cat php-phpixie/deploy/php-fpm.pid )", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1