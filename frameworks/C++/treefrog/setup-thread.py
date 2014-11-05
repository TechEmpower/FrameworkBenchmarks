
import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

##############
# start(args)
##############
def start(args, logfile, errfile):
  setup_util.replace_text("treefrog/config/database.ini", "HostName=.*", "HostName=" + args.database_host)
  setup_util.replace_text("treefrog/config/application.ini", "MultiProcessingModule=.*", "MultiProcessingModule=thread")

  # 1. Generate Makefile
  # 2. Compile applicaton
  # 3. Clean log files
  # 4. Start TreeFrog
  try:
    subprocess.check_call("qmake -r CONFIG+=release", shell=True, cwd="treefrog", stderr=errfile, stdout=logfile)
    subprocess.check_call("make clean", shell=True, cwd="treefrog", stderr=errfile, stdout=logfile)
    subprocess.check_call("make -j8", shell=True, cwd="treefrog", stderr=errfile, stdout=logfile)
    subprocess.check_call("rm -f log/*.log", shell=True, cwd="treefrog", stderr=errfile, stdout=logfile)
    subprocess.check_call("treefrog -d $TROOT", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop(logfile, errfile):
  try:
    subprocess.call("treefrog -k abort $TROOT", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
