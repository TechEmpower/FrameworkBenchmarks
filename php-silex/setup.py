import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args):
  setup_util.replace_text("php-silex/web/index.php", "192.168.100.102", "" + args.database_host + "")
  setup_util.replace_text("php-silex/deploy/php-silex", "\".*\/FrameworkBenchmarks", "\"" + home + "/FrameworkBenchmarks")
  setup_util.replace_text("php-silex/deploy/php-silex", "Directory .*\/FrameworkBenchmarks", "Directory " + home + "/FrameworkBenchmarks")
  setup_util.replace_text("php-silex/deploy/nginx.conf", "root .*\/FrameworkBenchmarks", "root " + home + "/FrameworkBenchmarks")

  try:
    #subprocess.check_call("sudo cp cake/deploy/cake /etc/apache2/sites-available/", shell=True)
    #subprocess.check_call("sudo a2ensite cake", shell=True)
    #subprocess.check_call("sudo chown -R www-data:www-data cake", shell=True)
    #subprocess.check_call("sudo /etc/init.d/apache2 start", shell=True)
    subprocess.check_call("composer.phar install --optimize-autoloader", shell=True, cwd="php-silex")        
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf -g " + home + "/FrameworkBenchmarks/php-silex/deploy/php-fpm.pid", shell=True)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/php-silex/deploy/nginx.conf", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    subprocess.call("sudo kill -QUIT $( cat php-silex/deploy/php-fpm.pid )", shell=True)
    #subprocess.check_call("sudo a2dissite cake", shell=True)
    #subprocess.check_call("sudo /etc/init.d/apache2 stop", shell=True)
    #subprocess.check_call("sudo chown -R $USER:$USER cake", shell=True)    
    return 0
  except subprocess.CalledProcessError:
    return 1
