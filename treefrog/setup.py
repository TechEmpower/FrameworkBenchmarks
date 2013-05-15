
import subprocess
import sys
import setup_util
from os.path import expanduser

home = expanduser("~")

##############
# start(args)
##############
def start(args):
  setup_util.replace_text("treefrog/config/database.ini", "HostName=.*", "HostName=" + args.database_host)

  # 1. Generate Makefile
  # 2. Compile applicaton
  # 3. Clean log files
  # 4. Start TreeFrog
  try:
    subprocess.check_call("qmake -r CONFIG+=release", shell=True, cwd="treefrog")
    subprocess.check_call("make", shell=True, cwd="treefrog")
    subprocess.check_call("rm -f log/*.log", shell=True, cwd="treefrog")
    subprocess.check_call("treefrog -d " + home + "/FrameworkBenchmarks/treefrog", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop():
  try:
    subprocess.call("treefrog -k abort " + home + "/FrameworkBenchmarks/treefrog", shell=True)
    return 0
  except subprocess.CalledProcessError:
    return 1
