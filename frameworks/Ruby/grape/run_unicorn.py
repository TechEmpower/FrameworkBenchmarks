import helper
from helper import Command

def start(args, logfile, errfile):
  helper.set_database_host(args)
  commands = [
    Command("rvm ruby-2.0.0-p0 do bundle", True),
    Command("rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb", False)
  ]

  return helper.run(commands, logfile, errfile, args.troot)

def stop(logfile, errfile):
  return helper.stop('unicorn', logfile, errfile)
