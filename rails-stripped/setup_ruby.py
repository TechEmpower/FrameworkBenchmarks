
import subprocess
import sys
import setup_util

def start(args):
  setup_util.replace_text("rails-stripped/config/database-ruby.yml", "host: .*", "host: " + args.database_host)
  
  try:
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle install --gemfile=Gemfile-ruby", shell=True, cwd="rails-stripped")
    subprocess.check_call("cp Gemfile-ruby Gemfile", shell=True, cwd="rails-stripped")
    subprocess.check_call("cp Gemfile-ruby.lock Gemfile.lock", shell=True, cwd="rails-stripped")
    subprocess.check_call("cp config/database-ruby.yml config/database.yml", shell=True, cwd="rails-stripped")
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle exec passenger start -p 8080 -d -e production --pid-file=$HOME/FrameworkBenchmarks/rails/rails.pid --nginx-version=1.2.7 --max-pool-size=24", shell=True, cwd="rails-stripped")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle exec passenger stop --pid-file=$HOME/FrameworkBenchmarks/rails/rails.pid", shell=True, cwd='rails-stripped')
    subprocess.check_call("rm Gemfile", shell=True, cwd="rails-stripped")
    subprocess.check_call("rm Gemfile.lock", shell=True, cwd="rails-stripped")
    return 0
  except subprocess.CalledProcessError:
    return 1
