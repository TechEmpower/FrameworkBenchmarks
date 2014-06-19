import subprocess
import sys
import setup_util
from os.path import expanduser
import os
import getpass

home = expanduser("~")

def start(args, logfile, errfile):
  setup_util.replace_text("plack/app.psgi", "localhost", ""+ args.database_host +"")
  setup_util.replace_text("plack/nginx.conf", "USR", getpass.getuser())
  setup_util.replace_text("plack/nginx.conf", "server unix:.*\/FrameworkBenchmarks", "server unix:" + home + "/FrameworkBenchmarks")
  try:
    subprocess.check_call("curl -L http://cpanmin.us | perl - App::cpanminus", shell=True, cwd="plack", stderr=errfile, stdout=logfile)
    subprocess.check_call("cpanm --notest --no-man-pages --installdeps "+home+"/FrameworkBenchmarks/plack", shell=True, cwd="plack", stderr=errfile, stdout=logfile)
    subprocess.Popen("start_server --backlog=16384 --pid-file="+home+"/FrameworkBenchmarks/plack/app.pid --path="+home+"/FrameworkBenchmarks/plack/app.sock -- plackup -E production -s Starlet --max-keepalive-reqs 1000 --max-reqs-per-child 50000 --min-reqs-per-child 40000 --max-workers=" + str(args.max_threads+128) + " -a "+home+"/FrameworkBenchmarks/plack/app.psgi", shell=True, cwd="plack", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/plack/nginx.conf", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.call('kill -TERM $(cat '+home+"/FrameworkBenchmarks/plack/app.pid)", shell=True, cwd="plack", stderr=errfile, stdout=logfile)
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1

