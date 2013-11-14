import subprocess
import sys
import setup_util
import os

def start(args, logfile, errfile):
  setup_util.replace_text("lapis/nginx.conf", "CWD", os.getcwd())
  setup_util.replace_text("lapis/nginx.conf", "DBHOSTNAME", args.database_host)
  subprocess.Popen('/usr/local/openresty/nginx/sbin/nginx -c `pwd`/nginx.conf -g "worker_processes ' + str((args.max_threads)) + ';"', shell=True, cwd="lapis", stderr=errfile, stdout=logfile)

  return 0

def stop(logfile, errfile):
  subprocess.Popen('/usr/local/openresty/nginx/sbin/nginx -c `pwd`/nginx.conf -s stop', shell=True, cwd="lapis", stderr=errfile, stdout=logfile)

  return 0
