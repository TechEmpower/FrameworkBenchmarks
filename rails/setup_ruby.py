
import subprocess
import sys
import re
import os
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args, logfile):
  setup_util.replace_text("rails/config/database-ruby.yml", "host: .*", "host: " + args.database_host)
  try:
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle install --gemfile=Gemfile-ruby", shell=True, cwd="rails", stderr=logfile, stdout=logfile)
    subprocess.check_call("cp Gemfile-ruby Gemfile", shell=True, cwd="rails", stderr=logfile, stdout=logfile)
    subprocess.check_call("cp Gemfile-ruby.lock Gemfile.lock", shell=True, cwd="rails", stderr=logfile, stdout=logfile)
    subprocess.check_call("cp config/database-ruby.yml config/database.yml", shell=True, cwd="rails", stderr=logfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c " + home + "/FrameworkBenchmarks/rails/config/nginx.conf", shell=True, stderr=logfile, stdout=logfile)
    subprocess.Popen("rvm ruby-2.0.0-p0 do bundle exec unicorn_rails -E production -c config/unicorn.rb", shell=True, cwd="rails", stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  subprocess.call("sudo /usr/local/nginx/sbin/nginx -s stop", shell=True, stderr=logfile, stdout=logfile)
  try:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'unicorn' in line and 'master' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
    # subprocess.check_call("rvm ruby-2.0.0-p0 do bundle exec passenger stop --pid-file=$HOME/FrameworkBenchmarks/rack/rack.pid", shell=True, cwd='rack')
    subprocess.check_call("rm Gemfile", shell=True, cwd="rails")
    subprocess.check_call("rm Gemfile.lock", shell=True, cwd="rails")
    return 0
  except subprocess.CalledProcessError:
    return 1
