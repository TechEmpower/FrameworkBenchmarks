# ---------------------------------------------------------------------------------------------------------------------------------
# toolset/run-tests.py --install server --install-strategy pertest --install-error-action continue --test ULib --type all --verbose
# ---------------------------------------------------------------------------------------------------------------------------------
import os
import sys
import time
#import logging
import setup_util
import subprocess
import multiprocessing

#log = logging.getLogger('framework_test')

# --------------------------------------------------------------------------------------------------------
# TROOT - Path of this test's directory
# IROOT - Path of this test's install directory ($FWROOT/installs or $FWROOT/installs/pertest/<test-name>)
# --------------------------------------------------------------------------------------------------------
script = """
if [ -n "${TROOT}" ] && [ -n "${IROOT}" ]; then
  return 0
fi
return 1
"""

p = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
p.communicate(script)
if p.returncode != 0:
  print('ULib: TROOT and IROOT are EMPTY...')
# log.critical('ULib: TROOT and IROOT are EMPTY...')
else:
  script = """
. ${TROOT}/bash_profile.sh
if [ -x "${ULIB_ROOT}/bin/userver_tcp" ] && [ -e "${ULIB_DOCUMENT_ROOT}/db.so" ] && [ -f "${ULIB_ROOT}/benchmark.cfg" ]; then
  return 0
fi
return 1
"""

  p1 = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
  p1.communicate(script)
  if p1.returncode != 0:
    print('ULib install script is NOT BEEN CALLED, I call it now...')
#   log.critical('ULib install script is NOT BEEN CALLED, I call it now...')
    subprocess.call("${TROOT}/install.sh", shell=True, executable='/bin/bash')
    p2 = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
    p2.communicate(script)
    if p2.returncode != 0:
      print('ULib install script FAILED')
#     log.critical('ULib install script FAILED')
      ulib_root = subprocess.check_output('. ${TROOT}/bash_profile.sh; printf "$ULIB_ROOT" 2>/dev/null', shell=True, executable='/bin/bash')
      fprg = ulib_root + "/bin/userver_tcp"
      if not os.path.exists(fprg):
        print("ULib server program " + fprg + " NOT EXIST")
#       log.critical("ULib server program " + fprg + " NOT EXIST")
      fcfg = ulib_root + "/benchmark.cfg"
      if not os.path.exists(fcfg):
        print("ULib configuration file " + fcfg + " NOT EXIST")
#       log.critical("ULib configuration file " + fcfg + " NOT EXIST")
      fusp = subprocess.check_output('. ${TROOT}/bash_profile.sh; printf "$ULIB_DOCUMENT_ROOT" 2>/dev/null', shell=True, executable='/bin/bash') + "/db.so"
      if not os.path.exists(fusp):
        print("ULib usp page " + fusp + " NOT EXIST")
#       log.critical("ULib usp page " + fusp + " NOT EXIST")
      print('Aborting')
#     log.critical('Aborting')
      exit(1)

##############
# start(args)
##############
def start(args, logfile, errfile):
  try:
    ulib_root = subprocess.check_output('. ${TROOT}/bash_profile.sh; printf "$ULIB_ROOT" 2>/dev/null', shell=True, executable='/bin/bash')
    fprg = ulib_root + "/bin/userver_tcp"
    fcfg = ulib_root + "/benchmark.cfg"
    # 1. Change ULib Server configuration
    # I don't understand if the two approach are different...
    #threads = str(args.max_threads)
    PROCS = str(multiprocessing.cpu_count())
    setup_util.replace_text(fcfg, "PREFORK_CHILD .*", "PREFORK_CHILD " + PROCS)

    ulib_server_output = subprocess.check_output('. ${TROOT}/bash_profile.sh; printf "$ULIB_SERVER_OUTPUT" 2>/dev/null', shell=True, executable='/bin/bash')

    # 2. Start ULib Server (userver_tcp)
    print("trying to start ULib server " + fprg + " -c " + fcfg)
#   log.info("trying to start ULib server " + fprg + " -c " + fcfg)

    # sudo mysqlcheck -v -r -A -u benchmarkdbuser -p
    os.putenv("ORM_DRIVER","mysql")
    os.putenv("ORM_OPTION","host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world")
    os.putenv("UMEMPOOL", "1583,1507,-19,45,16458,523,-27,-14,27")

    # Run in the background, but keep stdout/stderr for easy debugging
    subprocess.Popen(                      fprg + " -c " + fcfg + " >" + ulib_server_output + " 2>&1", shell=True, stdout=logfile, stderr=errfile)
  # subprocess.Popen("UTRACE=\"0 50M\" " + fprg + " -c " + fcfg + " >" + ulib_server_output + " 2>&1", shell=True, stdout=logfile, stderr=errfile)

    print("ULib server STARTED")
#   log.info("ULib server STARTED")
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop(logfile, errfile):
  try:
    ulib_root = subprocess.check_output('. ${TROOT}/bash_profile.sh; printf "$ULIB_ROOT" 2>/dev/null', shell=True, executable='/bin/bash')
    # Stop ULib Server (userver_tcp)
    subprocess.check_call("kill -TERM $( cat " + ulib_root + "/userver_tcp.pid )", shell=True, stderr=errfile, stdout=logfile)
    time.sleep(2);
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
       if 'userver_tcp' in line:
         pid = int(line.split(None, 2)[1])
         os.kill(pid, 9)
    subprocess.call("rm -f " + ulib_root + "/userver_tcp.pid", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
