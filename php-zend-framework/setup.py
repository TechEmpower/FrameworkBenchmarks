
import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args):
  setup_util.replace_text("php-zend-framework/config/benchmarks.local.php", "mysql:dbname=hello_world;host=localhost", "mysql:dbname=hello_world;host=" + args.database_host)
  setup_util.replace_text("php-zend-framework/deploy/nginx.conf", "root .*\/FrameworkBenchmarks", "root " + home + "/FrameworkBenchmarks")

  try:
    subprocess.check_call("composer.phar install --optimize-autoloader", shell=True, cwd="php-zend-framework")
    subprocess.check_call("sudo chown -R www-data:www-data php-zend-framework", shell=True)
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf -g " + home + "/FrameworkBenchmarks/php-zend-framework/deploy/php-fpm.pid", shell=True)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/php-zend-framework/deploy/nginx.conf", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    subprocess.call("sudo kill -QUIT $( cat php-zend-framework/deploy/php-fpm.pid )", shell=True)
    subprocess.check_call("sudo chown -R $USER:$USER php-zend-framework", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
