import os
import setup_util

import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm 2.1.2 do bundle exec unicorn_rails -E production -c $TROOT/config/unicorn.rb"

  commands = [
    Command("sudo /usr/local/nginx/sbin/nginx -c $TROOT/config/nginx.conf", True),
    Command(start_server, False)
  ]

  return helper.run(commands, logfile, errfile, args.troot)

def stop(logfile, errfile):
  helper.run([Command("sudo /usr/local/nginx/sbin/nginx -s stop -c $TROOT/config/nginx.conf", True)], logfile, errfile, os.environ['TROOT'])
  return helper.stop('unicorn', logfile, errfile)
