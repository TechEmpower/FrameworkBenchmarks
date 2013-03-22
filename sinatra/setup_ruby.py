
import subprocess
import sys
import setup_util

def start(args):
  setup_util.replace_text("sinatra/hello_world.rb", ":host => '.*'", ":host => '" + args.database_host + "'")

  try:
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle install --gemfile=Gemfile-ruby", shell=True, cwd="sinatra")
    subprocess.check_call("cp Gemfile-ruby Gemfile", shell=True, cwd="sinatra")
    subprocess.check_call("cp Gemfile-ruby.lock Gemfile.lock", shell=True, cwd="sinatra")
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle exec passenger start -p 8080 -d -e production --pid-file=$HOME/FrameworkBenchmarks/sinatra/sinatra.pid --nginx-version=1.2.7 --max-pool-size=24", shell=True, cwd="sinatra")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle exec passenger stop --pid-file=$HOME/FrameworkBenchmarks/sinatra/sinatra.pid", shell=True, cwd='sinatra')
    subprocess.check_call("rm Gemfile", shell=True, cwd="sinatra")
    subprocess.check_call("rm Gemfile.lock", shell=True, cwd="sinatra")
    return 0
  except subprocess.CalledProcessError:
    return 1
