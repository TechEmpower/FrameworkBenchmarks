
import subprocess
import sys
import re

def start(args, logfile, errfile):

  try:
    subprocess.check_call("rvm jruby-1.7.4 do bundle install --gemfile=Gemfile-jruby", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("cp Gemfile-jruby Gemfile", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("cp Gemfile-jruby.lock Gemfile.lock", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("rvm jruby-1.7.4 do warble war", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("rm -rf $RESIN_HOME/webapps/*", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("cp rack.war $RESIN_HOME/webapps/rack.war", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("$RESIN_HOME/bin/resinctl start", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=errfile, stdout=logfile)
    subprocess.check_call("rm Gemfile", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("rm Gemfile.lock", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
