# -------------------------------------------------------
# toolset/run-tests.py --name ULib --test ULib --type all
# -------------------------------------------------------
# IROOT - Path of this test's install directory ($FWROOT/installs or $FWROOT/installs/pertest/<test-name>)
# TROOT - Path of this test's directory
# -------------------------------------------------------

import subprocess
import sys
import os
import time
import setup_util

script = """
if [ -n "${IROOT}" ] && [ -n "${TROOT}" ]; then
  return 0;
fi
return 1;
"""

p = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
p.communicate(script)
if p.returncode != 0:
  froot = setup_util.get_fwroot()
  iroot = froot + "/installs" 
  troot = froot + "/ULib"
else:
  iroot = subprocess.check_output('printf $IROOT 2>/dev/null', shell=True, executable='/bin/bash')
  troot = subprocess.check_output('printf $TROOT 2>/dev/null', shell=True, executable='/bin/bash')

fprg  = iroot + "/bin/userver_tcp"
fusp  = troot + "/www/db.so"
fconf = troot + "/benchmark.cfg"

##############
# start(args)
##############
def start(args, logfile, errfile):

  script = """
  if [ -z "${IROOT}" ] || [ -z "${TROOT}" ]; then
    return 0;
  fi
  if [ -x "${IROOT}/bin/userver_tcp" ] && [ -e "${TROOT}/www/db.so" ] && [ -f "${TROOT}/benchmark.cfg" ]; then
    return 0;
  fi
  return 1;
  """

  p = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
  p.communicate(script)
  if p.returncode != 0:
    print("\nstart: ULib install script failed\n")
    if not os.path.exists(fprg):
      print("\nstart: ULib server program " + fprg + " not exist\n")
    if not os.path.exists(fusp):
      print("\nstart: ULib usp page " + fusp + " not exist\n")
    if not os.path.exists(fconf):
      print("\nstart: ULib configuration file " + fconf + " not exist\n")

  # 3. Start ULib Server (userver_tcp)
  try:
    threads = str(args.max_threads)
    setup_util.replace_text(fconf, "PREFORK_CHILD *", "PREFORK_CHILD " + threads)

    print("\nstart: trying to start ULib server " + fprg + " -c " + fconf + "\n")

    os.putenv("ORM_DRIVER","mysql")
    os.putenv("ORM_OPTION","host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world")
    os.putenv("UMEMPOOL", "1583,1507,-19,45,16458,523,-27,-14,27")

    subprocess.check_call(fprg + " -c " + fconf + " >" + iroot + "/ULIB_SERVER_OUTPUT.txt 2>&1", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop(logfile, errfile):
  try:
    subprocess.check_call("kill -TERM $( cat " + iroot + "/userver_tcp.pid )", shell=True, stderr=errfile, stdout=logfile)
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
