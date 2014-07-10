import subprocess
import sys
import os
import time
import setup_util

from os.path import expanduser

home = expanduser("~")

##############
# start(args)
##############
def start(args, logfile, errfile):

  script = """
  DIR=$HOME/FrameworkBenchmarks
  PREFIX=$DIR/installs/ulib
  DOCUMENT_ROOT=$DIR/ULib/www
  if [ -x "$PREFIX/bin/userver_tcp" ] && [ -d "$DOCUMENT_ROOT" ] && [ -e "$DOCUMENT_ROOT/db.so" ] && [ -f $DIR/ULib/benchmark.cfg ]; then
    return 0;
  fi
  return 1;
  """

  p = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
  p.communicate(script)
  if p.returncode != 0:
    print("\nstart: ULib install script failed\n")

  # 3. Start ULib Server (userver_tcp)
  try:
    fconf = home + "/FrameworkBenchmarks/ULib/benchmark.cfg"
    if not os.path.exists(fconf):
      print("\nstart: ULib configuration file " + fconf + " not exist\n")

    threads = str(args.max_threads)
    setup_util.replace_text(fconf, "PREFORK_CHILD *", "PREFORK_CHILD " + threads)

    fprg = home + "/FrameworkBenchmarks/installs/ulib/bin/userver_tcp"
    if not os.path.exists(fprg):
      print("\nstart: ULib server program " + fprg + " not exist\n")

    os.putenv("ORM_DRIVER","mysql")
    os.putenv("ORM_OPTION","host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world")
    os.putenv("UMEMPOOL", "1583,1507,-19,45,16458,523,-27,-14,27")

    print("\nstart: trying to start ULib server " + fprg + " -c " + fconf + "\n")

    subprocess.check_call(fprg + " -c " + fconf + " >" + home + "/FrameworkBenchmarks/installs/ulib/ULIB_SERVER_OUTPUT.txt 2>&1", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop(logfile, errfile):
  try:
    subprocess.check_call("kill -TERM $( cat " + home + "/FrameworkBenchmarks/ULib/userver_tcp.pid )", shell=True, stderr=errfile, stdout=logfile)
    time.sleep(2);
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
       if 'userver_tcp' in line:
         pid = int(line.split(None, 2)[1])
         os.kill(pid, 9)
    return 0
  except subprocess.CalledProcessError:
    return 1
