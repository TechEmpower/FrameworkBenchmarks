import subprocess
import sys
import setup_util
from os.path import expanduser
import os
import getpass

def start(args, logfile, errfile):
  setup_util.replace_text("plack/app.psgi", "localhost", args.database_host)
  setup_util.replace_text("plack/nginx.conf", "USR", getpass.getuser())
  setup_util.replace_text("plack/nginx.conf", "server unix:.*\/FrameworkBenchmarks", "server unix:" + args.fwroot)
  try:
    subprocess.Popen("start_server --backlog=16384 --pid-file=$TROOT/app.pid --path=$TROOT/app.sock -- plackup -E production -s Starlet --max-keepalive-reqs 1000 --max-reqs-per-child 50000 --min-reqs-per-child 40000 --max-workers=" + str(args.max_threads+4) + " -a $TROOT/app.psgi", shell=True, cwd="plack", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/nginx.conf", shell=True,stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.call('kill -TERM $(cat $TROOT/app.pid)', shell=True, stderr=errfile, stdout=logfile)
    subprocess.call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/nginx.conf -s stop", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

