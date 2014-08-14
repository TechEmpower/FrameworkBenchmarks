import subprocess
import sys
import setup_util

def start(args, logfile, errfile):
  setup_util.replace_text("cowboy/src/hello_world_app.erl", "\"benchmarkdbpass\", \".*\", 3306", "\"benchmarkdbpass\", \"" + args.database_host + "\", 3306")

  try:
    subprocess.check_call("rm -rf deps/*", shell=True, cwd="cowboy", stderr=errfile, stdout=logfile)
    subprocess.check_call("./rebar get-deps", shell=True, cwd="cowboy", stderr=errfile, stdout=logfile)
    subprocess.check_call("./rebar compile", shell=True, cwd="cowboy", stderr=errfile, stdout=logfile)
    subprocess.check_call("erl -pa ebin deps/*/ebin +sbwt very_long +swt very_low -s hello_world -noshell -detached", shell=True, cwd="cowboy", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.check_call("killall beam.smp", shell=True, cwd="/usr/bin")
    return 0
  except subprocess.CalledProcessError:
    return 1
