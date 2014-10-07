import subprocess
import sys
import setup_util
from os.path import expanduser

def start(args, logfile, errfile):
  
  setup_util.replace_text("gemini/Docroot/WEB-INF/GeminiHello.conf", "db.ConnectString = .*:3306", "db.ConnectString = " + args.database_host + ":3306")
  setup_util.replace_text("gemini/Docroot/WEB-INF/resin.xml", "root-directory=\".*\/FrameworkBenchmarks/gemini", "root-directory=\"%s" % args.troot)
  
  try:
    subprocess.call("mkdir -p classes", shell=True, cwd="gemini/Docroot/WEB-INF", stderr=errfile, stdout=logfile)
    subprocess.check_call("ant compile", shell=True, cwd="gemini", stderr=errfile, stdout=logfile)
    subprocess.check_call("$RESIN_HOME/bin/resinctl -conf $TROOT/Docroot/WEB-INF/resin.xml start", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile, errfile):
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1