
import subprocess
import sys
import setup_util

def start(args):
  setup_util.replace_text("rails-optimized/config/database-jruby.yml", "host: .*", "host: " + args.database_host)
  setup_util.replace_text("rails-optimized/resin-web.xml", "mysql:\/\/.*:3306", "mysql://" + args.database_host + ":3306")

  try:
    subprocess.check_call("rvm jruby-1.7.3 do bundle install --gemfile=Gemfile-jruby", shell=True, cwd="rails-optimized")
    subprocess.check_call("cp Gemfile-jruby Gemfile", shell=True, cwd="rails-optimized")
    subprocess.check_call("cp Gemfile-jruby.lock Gemfile.lock", shell=True, cwd="rails-optimized")
    subprocess.check_call("cp config/database-jruby.yml config/database.yml", shell=True, cwd="rails-optimized")
    subprocess.check_call("rvm jruby-1.7.3 do warble war", shell=True, cwd="rails-optimized")
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True)
    subprocess.check_call("cp rails-optimized.war $RESIN_HOME/webapps/rails.war", shell=True, cwd="rails-optimized")
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop():
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True)
    subprocess.check_call("rm Gemfile", shell=True, cwd="rails-optimized")
    subprocess.check_call("rm Gemfile.lock", shell=True, cwd="rails-optimized")
    return 0
  except subprocess.CalledProcessError:
    return 1
