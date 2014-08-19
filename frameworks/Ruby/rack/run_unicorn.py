import helper
from helper import Command

from os.path import expanduser
home = expanduser("~")

def start(args, logfile, errfile):
  helper.set_database_host(args)
  commands = [
    Command("rvm ruby-2.0.0-p0 do bundle", True),
    Command("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/frameworks/Ruby/rack/config/nginx.conf", True),
    Command("rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb", False)
  ]

  return helper.run(commands, logfile, errfile)

def stop(logfile, errfile):
  helper.run([Command("sudo /usr/local/nginx/sbin/nginx -s stop", True)], logfile, errfile)
  return helper.stop('unicorn', logfile, errfile)