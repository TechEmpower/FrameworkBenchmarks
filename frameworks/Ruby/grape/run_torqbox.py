import helper
from helper import Command

def start(args, logfile, errfile):
  helper.set_database_host(args)
  commands = [
    Command("rvm jruby-1.7.8 do bundle", True),
    Command("rvm jruby-1.7.8 do bundle exec torqbox -b 0.0.0.0 -E production", False)
  ]

  return helper.run(commands, logfile, errfile, args.troot)

def stop(logfile, errfile):
  return helper.stop('torqbox', logfile, errfile)
