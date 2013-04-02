
import subprocess
import sys
import setup_util
import os
from os.path import expanduser

home = expanduser("~")

def start(args):
  setup_util.replace_text("php/dborm.php", "@.*\/hello_world", "@" + args.database_host + "/hello_world")
  setup_util.replace_text("php/dbraw.php", "host=.*;", "host=" + args.database_host + ";")
  setup_util.replace_text("php/deploy/php", "\".*\/FrameworkBenchmarks", "\"" + home + "/FrameworkBenchmarks")
  setup_util.replace_text("php/deploy/php", "Directory .*\/FrameworkBenchmarks", "Directory " + home + "/FrameworkBenchmarks")
  
  try:
    #subprocess.check_call("sudo cp php/deploy/php /etc/apache2/sites-available/", shell=True)
    #subprocess.check_call("sudo a2ensite php", shell=True)
    #subprocess.check_call("sudo chown -R www-data:www-data php", shell=True)
    #subprocess.check_call("sudo /etc/init.d/apache2 start", shell=True)
    subprocess.check_call("sudo php-fpm --fpm-config config/php-fpm.conf", shell=True)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "FrameworkBenchmarks/php/deploy/nginx.conf", shell=True)
    
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'php-fpm' in line:
        try:
          pid = int(line.split(None, 2)[1])
          os.kill(pid, 9)
        except OSError:
          pass
    
    return 0
  except subprocess.CalledProcessError:
    return 1
