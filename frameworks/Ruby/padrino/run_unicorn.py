import os

import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb"
  commands = [Command("sudo /usr/local/nginx/sbin/nginx -c $TROOT/config/nginx.conf", True),
              Command(start_server, False)]
  return helper.run(commands, logfile, errfile, args.troot)

def stop(logfile, errfile):
  command = Command("sudo /usr/local/nginx/sbin/nginx -c $TROOT/config/nginx.conf -s stop ", True)
  helper.run([command], logfile, errfile, os.environ['TROOT'])
  return helper.stop('unicorn', logfile, errfile)
