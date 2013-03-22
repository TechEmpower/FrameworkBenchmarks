
import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args):
  setup_util.replace_text("cake/app/Config/database.php", "'host' => '.*',", "'host' => '" + args.database_host + "',")
  setup_util.replace_text("cake/deploy/cake", "\".*\/FrameworkBenchmarks", "\"" + home + "/FrameworkBenchmarks")
  setup_util.replace_text("cake/deploy/cake", "Directory .*\/FrameworkBenchmarks", "Directory " + home + "/FrameworkBenchmarks")

  try:
    subprocess.check_call("sudo cp cake/deploy/cake /etc/apache2/sites-available/", shell=True)
    subprocess.check_call("sudo a2ensite cake", shell=True)
    subprocess.check_call("sudo chown -R www-data:www-data cake", shell=True)
    subprocess.check_call("sudo /etc/init.d/apache2 start", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("sudo a2dissite cake", shell=True)
    subprocess.check_call("sudo /etc/init.d/apache2 stop", shell=True)
    subprocess.check_call("sudo chown -R $USER:$USER cake", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
