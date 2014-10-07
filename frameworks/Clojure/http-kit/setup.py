
import subprocess
import sys
import setup_util

def start(args, logfile, errfile):

  try:
    subprocess.check_call("lein clean", shell=True, cwd="http-kit/hello", stderr=errfile, stdout=logfile)
    subprocess.check_call("lein deps", shell=True, cwd="http-kit/hello", stderr=errfile, stdout=logfile)
    subprocess.check_call("rm -rf target", shell=True, cwd="http-kit/hello")
    # pack all dependencies into a single jar: target/http-kit-standalone.jar
    subprocess.check_call("lein uberjar", shell=True, cwd="http-kit/hello", stderr=errfile, stdout=logfile)
    # -server is much faster
    # 'lein run' passes '-client -XX:+TieredCompilation -XX:TieredStopAtLevel=1' which make it starts fast, but runs slow
    command = "java -server -jar target/http-kit-standalone.jar --db-host " + args.database_host
    subprocess.Popen(command, shell=True, cwd="http-kit/hello", stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

def stop(logfile, errfile):
  try:
    # listen on 8080
    subprocess.check_call("lsof -t -sTCP:LISTEN -i:8080 | xargs kill", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
