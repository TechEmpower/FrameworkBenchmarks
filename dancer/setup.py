import subprocess
import sys
import setup_util
from os.path import expanduser
import os
import getpass

home = expanduser("~")

def start(args):
  setup_util.replace_text("dancer/app.pl", "localhost", ""+ args.database_host +"")
  setup_util.replace_text("dancer/nginx.conf", "USR", getpass.getuser())

  try:
    subprocess.Popen("plackup -E production -s Starman --workers=2 -l " + home + "/FrameworkBenchmarks/dancer/frameworks-benchmark.sock -a ./app.pl", shell=True, cwd="dancer")
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/dancer/nginx.conf", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'plackup' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
    return 0
  except subprocess.CalledProcessError:
    return 1
