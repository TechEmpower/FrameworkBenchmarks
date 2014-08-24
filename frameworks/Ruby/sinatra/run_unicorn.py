import os
import setup_util

import helper
from helper import Command

def start(args, logfile, errfile):
  #setup_util.replace_text("sinatra/config/nginx.conf", "/path/to/app/current", args.troot)

  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb"

  commands = [
    Command("rvm ruby-2.0.0-p0 do bundle --jobs 4", True),
    Command("sudo /usr/local/nginx/sbin/nginx -c $TROOT/config/nginx.conf", True),
    Command(start_server, False)
  ]

  return helper.run(commands, logfile, errfile, args.troot)

def stop(logfile, errfile):
  helper.run([Command("sudo /usr/local/nginx/sbin/nginx -s stop -c $TROOT/config/nginx.conf", True)], logfile, errfile, os.environ['TROOT'])
  return helper.stop('unicorn', logfile, errfile)
