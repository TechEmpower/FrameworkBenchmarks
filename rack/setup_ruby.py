
import subprocess
import sys
import re

def start(args):

  try:
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle install --gemfile=Gemfile-ruby", shell=True, cwd="rack")
    subprocess.check_call("cp Gemfile-ruby Gemfile", shell=True, cwd="rack")
    subprocess.check_call("cp Gemfile-ruby.lock Gemfile.lock", shell=True, cwd="rack")
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle exec passenger start -p 8080 -d -e production --pid-file=$HOME/FrameworkBenchmarks/rack/rack.pid --nginx-version=1.2.7 --max-pool-size=24", shell=True, cwd="rack")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle exec passenger stop --pid-file=$HOME/FrameworkBenchmarks/rack/rack.pid", shell=True, cwd='rack')
    subprocess.check_call("rm Gemfile", shell=True, cwd="rack")
    subprocess.check_call("rm Gemfile.lock", shell=True, cwd="rack")
    return 0
  except subprocess.CalledProcessError:
    return 1
