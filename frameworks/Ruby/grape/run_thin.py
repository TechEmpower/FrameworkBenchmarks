import helper
from helper import Command

def start(args, logfile, errfile):
  helper.set_database_host(args)
  command = Command("rvm ruby-2.0.0-p0 do bundle exec thin start -C config/thin.yml", False)
  return helper.run([command], logfile, errfile, args.troot)

def stop(logfile, errfile):
  helper.run([Command('rm -rf tmp/*', True)], logfile, errfile, '.')  
  return helper.stop('thin', logfile, errfile)
