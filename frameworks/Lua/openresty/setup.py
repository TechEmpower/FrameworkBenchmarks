import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("openresty/nginx.conf", "CWD", args.troot)
  setup_util.replace_text("openresty/app.lua", "DBHOSTNAME", args.database_host)
  subprocess.Popen('sudo /usr/local/openresty/nginx/sbin/nginx -c $TROOT/nginx.conf -g "worker_processes ' + str((args.max_threads)) + ';"', shell=True, cwd="openresty", stderr=errfile, stdout=logfile)

  return 0

def stop(logfile, errfile):
  subprocess.Popen('sudo /usr/local/openresty/nginx/sbin/nginx -c $TROOT/nginx.conf -s stop', shell=True, cwd="openresty", stderr=errfile, stdout=logfile)

  return 0
