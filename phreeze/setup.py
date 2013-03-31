import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args):
  setup_util.replace_text("phreeze/index.php", "localhost:3306", "" + args.database_host + ":3306")
  setup_util.replace_text("phreeze/deploy/phreeze", "\".*\/FrameworkBenchmarks", "\"" + home + "/FrameworkBenchmarks")
  setup_util.replace_text("phreeze/deploy/phreeze", "Directory .*\/FrameworkBenchmarks", "Directory " + home + "/FrameworkBenchmarks")

  try:
    subprocess.check_call("sudo cp phreeze /etc/apache2/sites-available/", shell=True)
    subprocess.check_call("sudo a2ensite phreeze", shell=True)
    subprocess.check_call("sudo chown -R www-data:www-data phreeze", shell=True)
    subprocess.check_call("sudo /etc/init.d/apache2 start", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("sudo a2dissite phreeze", shell=True)
    subprocess.check_call("sudo /etc/init.d/apache2 stop", shell=True)
    subprocess.check_call("sudo chown -R $USER:$USER phreeze", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1