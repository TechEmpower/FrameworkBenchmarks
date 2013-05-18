import subprocess
import sys
import setup_util
import os

def start(args):
  setup_util.replace_text("plack/app.psgi", "localhost", ""+ args.database_host +"")
  try:
    subprocess.check_call("curl -L http://cpanmin.us | perl - App::cpanminus", shell=True, cwd="plack")
    subprocess.check_call("cpanm --installdeps .", shell=True, cwd="plack")
    pid = subprocess.Popen("plackup -E production -s Monoceros -l :8080 --max-workers=" + str(args.max_threads) + " app.psgi", shell=True, cwd="plack").pid
    open('plack/app.pid', 'w').write(str(pid))
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.Popen("kill -TERM $(ps --ppid `cat app.pid` -o pid --no-header)", shell=True, cwd="plack")
    return 0
  except subprocess.CalledProcessError:
    return 1
