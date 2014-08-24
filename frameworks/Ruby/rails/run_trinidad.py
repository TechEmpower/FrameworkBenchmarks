import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm jruby-1.7.13 do bundle exec trinidad --config config/trinidad.yml"
  return helper.run([Command(start_server, False)], logfile, errfile, args.troot)

def stop(logfile, errfile):
  return helper.stop('trinidad', logfile, errfile)
