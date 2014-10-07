import helper
from helper import Command

def start(args, logfile, errfile):
  helper.set_database_host(args)
  command = Command("rvm jruby-1.7.8 do bundle exec trinidad --config config/trinidad.yml", False)
  return helper.run([command], logfile, errfile, args.troot)

def stop(logfile, errfile):
  return helper.stop('trinidad', logfile, errfile)
