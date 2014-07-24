# -------------------------------------------------------
# toolset/run-tests.py --name ULib --test ULib --type all
# -------------------------------------------------------
import os
import sys
import time
#import logging
import setup_util
import subprocess
import multiprocessing

#log = logging.getLogger('framework_test')

subprocess.call("source ${HOME}/FrameworkBenchmarks/ULib/bash_profile.sh", shell=True, executable='/bin/bash')

ulib_root          = subprocess.check_output('printf "$ULIB_ROOT"          2>/dev/null', shell=True, executable='/bin/bash')
ulib_document_root = subprocess.check_output('printf "$ULIB_DOCUMENT_ROOT" 2>/dev/null', shell=True, executable='/bin/bash')

fcfg = ulib_root + "/benchmark.cfg"
fprg = ulib_root + "/bin/userver_tcp"
fusp = ulib_document_root + "/db.so"

script = """
if [ -x "${ULIB_ROOT}/bin/userver_tcp" ] && [ -e "${ULIB_DOCUMENT_ROOT}/db.so" ] && [ -f "${ULIB_ROOT}/benchmark.cfg" ]; then
  return 0;
fi
return 1;
"""

p = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
p.communicate(script)
if p.returncode != 0:
# log.critical('ULib install script FAILED')
  print('ULib install script FAILED')
  if not os.path.exists(fprg):
#   log.critical("ULib server program " + fprg + " NOT EXIST")
    print("ULib server program " + fprg + " NOT EXIST")
  if not os.path.exists(fusp):
#   log.critical("ULib usp page " + fusp + " NOT EXIST")
    print("ULib usp page " + fusp + " NOT EXIST")
  if not os.path.exists(fcfg):
#   log.critical("ULib configuration file " + fcfg + " NOT EXIST")
    print("ULib configuration file " + fcfg + " NOT EXIST")
# log.critical('Aborting')
  print('Aborting')
  exit(1)

##############
# start(args)
##############
def start(args, logfile, errfile):
  try:
    # 1. Change ULib Server configuration
    #threads = str(args.max_threads)
    PROCS = str(multiprocessing.cpu_count())
    setup_util.replace_text(fcfg, "PREFORK_CHILD *", "PREFORK_CHILD " + PROCS)

    # 2. Start ULib Server (userver_tcp)
#   log.info("trying to start ULib server " + fprg + " -c " + fcfg)
    print("trying to start ULib server " + fprg + " -c " + fcfg)

    os.putenv("ORM_DRIVER","mysql")
    os.putenv("ORM_OPTION","host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world")
    os.putenv("UMEMPOOL", "1583,1507,-19,45,16458,523,-27,-14,27")

    # Run in the background, but keep stdout/stderr for easy debugging
    subprocess.Popen(fprg + " -c " + fcfg + " >$ULIB_SERVER_OUTPUT 2>&1", shell=True, stdout=logfile, stderr=errfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop(logfile, errfile):
  try:
    # Stop ULib Server (userver_tcp)
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
