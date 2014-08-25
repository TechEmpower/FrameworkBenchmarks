import os

import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm ruby-2.1.2 do bundle exec thin start -C config/thin.yml"
  return helper.run([Command(start_server, False)], logfile, errfile, args.troot)

def stop(logfile, errfile):
  helper.run([Command('rm -rf tmp/*', True)], logfile, errfile, os.environ['TROOT'])
  return helper.stop('thin', logfile, errfile)
