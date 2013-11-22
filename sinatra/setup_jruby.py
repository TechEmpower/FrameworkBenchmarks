
import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("sinatra/hello_world.rb", ":host => '.*'", ":host => '" + args.database_host + "'")
  try:
    subprocess.check_call("rvm jruby-1.7.8 do bundle install --gemfile=Gemfile-jruby", shell=True, cwd="sinatra", stderr=errfile, stdout=logfile)
    subprocess.check_call("cp Gemfile-jruby Gemfile", shell=True, cwd="sinatra", stderr=errfile, stdout=logfile)
    subprocess.check_call("cp Gemfile-jruby.lock Gemfile.lock", shell=True, cwd="sinatra", stderr=errfile, stdout=logfile)
    subprocess.Popen("rvm jruby-1.7.8 do bundle exec torqbox -E production", shell=True, cwd="sinatra", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
      if 'torqbox' in line:
        pid = int(line.split(None, 2)[1])
        os.kill(pid, 9)
    subprocess.check_call("rm Gemfile", shell=True, cwd="sinatra", stderr=errfile, stdout=logfile)
    subprocess.check_call("rm Gemfile.lock", shell=True, cwd="sinatra", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
