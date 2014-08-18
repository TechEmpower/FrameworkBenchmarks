
import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("php/dborm.php", "@.*\/hello_world", "@" + args.database_host + "/hello_world")
  setup_util.replace_text("php/dbraw.php", "host=.*;", "host=" + args.database_host + ";")
  setup_util.replace_text("php/updateraw.php", "host=.*;", "host=" + args.database_host + ";")
  setup_util.replace_text("php/fortune.php", "host=.*;dbname", "host=" + args.database_host + ";dbname")
  
  setup_util.replace_text("php/deploy/php", "\".*\/FrameworkBenchmarks/php", "\"" + args.troot)
  setup_util.replace_text("php/deploy/php", "Directory .*\/FrameworkBenchmarks/php", "Directory " + args.troot)
  setup_util.replace_text("php/deploy/nginx.conf", "root .*\/FrameworkBenchmarks/php", "root " + args.troot)
  
  try:
    if os.name == 'nt':
      subprocess.check_call('appcmd add site /name:PHP /bindings:http/*:8080: /physicalPath:"C:\\FrameworkBenchmarks\\php"', shell=True, stderr=errfile, stdout=logfile)
      return 0
    
    #subprocess.check_call("sudo cp php/deploy/php /etc/apache2/sites-available/", shell=True)
    #subprocess.check_call("sudo a2ensite php", shell=True)
    #subprocess.check_call("sudo chown -R www-data:www-data php", shell=True)
    #subprocess.check_call("sudo /etc/init.d/apache2 start", shell=True)
    subprocess.check_call("sudo $PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/deploy/nginx.conf", shell=True, stderr=errfile, stdout=logfile)
    
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    if os.name == 'nt':
      subprocess.check_call('appcmd delete site PHP', shell=True, stderr=errfile, stdout=logfile)
      return 0
    
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("sudo kill -QUIT $( cat $TROOT/deploy/php-fpm.pid )", shell=True, stderr=errfile, stdout=logfile)
    
    return 0
  except subprocess.CalledProcessError:
    return 1
