
import subprocess
import sys
import re
import os

def start(args, logfile, errfile):

  try:
    subprocess.check_call("cp Gemfile-ruby Gemfile", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("cp Gemfile-ruby.lock Gemfile.lock", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/config/nginx.conf", shell=True, stderr=errfile, stdout=logfile)
    subprocess.Popen("rvm ruby-2.0.0-p0 do bundle exec unicorn -E production -c $TROOT/config/unicorn.rb", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  subprocess.call("sudo /usr/local/nginx/sbin/nginx -c $TROOT/config/nginx.conf -s stop", shell=True, stderr=errfile, stdout=logfile)
  try:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'unicorn' in line and 'master' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 15)
    subprocess.check_call("rm -f Gemfile", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    subprocess.check_call("rm -f Gemfile.lock", shell=True, cwd="rack", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
