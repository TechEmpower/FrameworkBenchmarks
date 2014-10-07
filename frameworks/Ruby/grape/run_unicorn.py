import helper
from helper import Command

def start(args, logfile, errfile):
  helper.set_database_host(args)
  commands = [Command("sudo /usr/local/nginx/sbin/nginx -c $TROOT/config/nginx.conf", True),
              Command("rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb", False)]

  return helper.run(commands, logfile, errfile, args.troot)

def stop(logfile, errfile):
  command = Command("sudo /usr/local/nginx/sbin/nginx -c $TROOT/config/nginx.conf -s stop ", True)
  helper.run([command], logfile, errfile, '.')  
  return helper.stop('unicorn', logfile, errfile)
