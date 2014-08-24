import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm ruby-2.1.2 do bundle exec puma -t 8:32 -w 8 --preload -b tcp://0.0.0.0:8080 -e production"
  return helper.run([Command(start_server, False)], logfile, errfile, args.troot)

def stop(logfile, errfile):
  return helper.stop('puma', logfile, errfile)
