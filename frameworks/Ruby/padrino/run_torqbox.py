import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm jruby-1.7.8 do bundle exec torqbox -b 0.0.0.0 -E production"
  return helper.run([Command(start_server, False)], logfile, errfile, args.troot)

def stop(logfile, errfile):
  return helper.stop('torqbox', logfile, errfile)
