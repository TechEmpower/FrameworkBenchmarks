import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args):
  setup_util.replace_text("php-pimf/app/config.app.php", "127.0.0.1", "" + args.database_host + "")
  setup_util.replace_text("php-pimf/deploy/php-pimf", "\".*\/FrameworkBenchmarks", "\"" + home + "/FrameworkBenchmarks")
  setup_util.replace_text("php-pimf/deploy/php-pimf", "Directory .*\/FrameworkBenchmarks", "Directory " + home + "/FrameworkBenchmarks")
  setup_util.replace_text("php-pimf/deploy/nginx_raw.conf", "root .*\/FrameworkBenchmarks", "root " + home + "/FrameworkBenchmarks")

  try:
    #subprocess.check_call("composer.phar install --optimize-autoloader", shell=True, cwd="php-pimf")
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf -g " + home + "/FrameworkBenchmarks/php-pimf/deploy/php-fpm.pid", shell=True)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/php-pimf/deploy/nginx_raw.conf", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop():
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    subprocess.call("sudo kill -QUIT $( cat php-pimf/deploy/php-fpm.pid )", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
