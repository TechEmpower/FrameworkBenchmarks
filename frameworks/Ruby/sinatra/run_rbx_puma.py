import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  command = Command(db_host + " rvm rbx-2.2.10 do bundle exec puma", False)
  return helper.run([command], logfile, errfile, args.troot)

def stop(logfile, errfile):
  return helper.stop('puma', logfile, errfile)
