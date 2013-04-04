
import subprocess
import sys
import re
import os

def start(args):

  try:
    subprocess.check_call("rvm ruby-2.0.0-p0 do bundle install --gemfile=Gemfile-ruby", shell=True, cwd="rack")
    subprocess.check_call("cp Gemfile-ruby Gemfile", shell=True, cwd="rack")
    subprocess.check_call("cp Gemfile-ruby.lock Gemfile.lock", shell=True, cwd="rack")
    subprocess.Popen("rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c config/unicorn.rb", shell=True, cwd="rack")
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'unicorn' in line and 'master' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
    # subprocess.check_call("rvm ruby-2.0.0-p0 do bundle exec passenger stop --pid-file=$HOME/FrameworkBenchmarks/rack/rack.pid", shell=True, cwd='rack')
    subprocess.check_call("rm Gemfile", shell=True, cwd="rack")
    subprocess.check_call("rm Gemfile.lock", shell=True, cwd="rack")
    return 0
  except subprocess.CalledProcessError:
    return 1
