import helper
from helper import Command

def start(args, logfile, errfile):
  helper.set_database_host(args)
  commands = [
    Command("rvm jruby-1.7.8 do bundle", True),
    Command("rvm jruby-1.7.8 do bundle exec puma -b tcp://0.0.0.0:8080 -e production", False)
  ]

  return helper.run(commands, logfile, errfile)

def stop(logfile, errfile):
  return helper.stop('puma', logfile, errfile)
