import os

import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm ruby-2.0.0-p0 do bundle exec thin start -C config/thin.yml"

  commands = [
    Command("rvm ruby-2.0.0-p0 do bundle --jobs 4", True),
    Command(start_server, False)
  ]

  return helper.run(commands, logfile, errfile, args.troot)

def stop(logfile, errfile):
  helper.run([Command('rm -rf tmp/*', True)], logfile, errfile, os.environ['TROOT'])
  return helper.stop('thin', logfile, errfile)
