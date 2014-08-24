import helper
from helper import Command

def start(args, logfile, errfile):
  db_host = "DB_HOST={0}".format(args.database_host or 'localhost')
  start_server = db_host + " rvm jruby-1.7.8 do bundle exec puma -C config/puma.rb"

  commands = [
    Command("rvm jruby-1.7.8 do bundle --jobs 4", True),
    Command(start_server, False)
  ]

  return helper.run(commands, logfile, errfile, args.troot)

def stop(logfile, errfile):
  return helper.stop('puma', logfile, errfile)
