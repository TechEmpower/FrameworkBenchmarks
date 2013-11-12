
import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

def start(args, logfile):
  setup_util.replace_text("gemini/Docroot/WEB-INF/GeminiHello.conf", "db.ConnectString = .*:3306", "db.ConnectString = " + args.database_host + ":3306")
  setup_util.replace_text("gemini/Docroot/WEB-INF/resin.xml", "root-directory=\".*\/FrameworkBenchmarks", "root-directory=\"" + home + "/FrameworkBenchmarks")
  
  try:
    subprocess.call("mkdir classes", shell=True, cwd="gemini/Docroot/WEB-INF", stderr=logfile, stdout=logfile)
    subprocess.check_call("ant compile", shell=True, cwd="gemini", stderr=logfile, stdout=logfile)
    subprocess.check_call("$RESIN_HOME/bin/resinctl -conf $HOME/FrameworkBenchmarks/gemini/Docroot/WEB-INF/resin.xml start", shell=True, stderr=out, stdout=out)
    return 0
  except subprocess.CalledProcessError:
    return 1
def stop(logfile):
  try:
    subprocess.check_call("$RESIN_HOME/bin/resinctl shutdown", shell=True, stderr=logfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
