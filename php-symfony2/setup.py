import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args, logfile, errfile):
  setup_util.replace_text("php-symfony2/app/config/parameters.yml", "database_host: .*", "database_host: " + args.database_host)
  setup_util.replace_text("php-symfony2/deploy/nginx.conf", "root .*\/FrameworkBenchmarks", "root " + home + "/FrameworkBenchmarks")

  try:
    subprocess.check_call("composer.phar install --optimize-autoloader", shell=True, cwd="php-symfony2", stderr=errfile, stdout=logfile)
    subprocess.check_call("php app/console cache:clear --env=prod --no-debug", shell=True, cwd="php-symfony2", stderr=errfile, stdout=logfile)
    subprocess.check_call("php app/console cache:warmup --env=prod --no-debug", shell=True, cwd="php-symfony2", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo chown -R www-data:www-data php-symfony2", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf -g " + home + "/FrameworkBenchmarks/php-symfony2/deploy/php-fpm.pid", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/php-symfony2/deploy/nginx.conf", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("sudo kill -QUIT $( cat php-symfony2/deploy/php-fpm.pid )", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo chown -R $USER:$USER php-symfony2", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
