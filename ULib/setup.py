# setup.py

# THIS CALLING IS WORKING
# ----------------------------------------------------------------------
# toolset/run-tests.py --install server --test ULib --type all --verbose
# ----------------------------------------------------------------------
# ....
# ULib: setup.py running - FWROOT is /home/tfb/FrameworkBenchmarks
# ....
# INFO:root:Running installation for ULib
# INSTALL: 
#    export TROOT=$FWROOT/ULib && 
#    export IROOT=$FWROOT/installs && 
#    . $FWROOT/toolset/setup/linux/bash_functions.sh && 
#    . $FWROOT/ULib/install.sh (cwd=$FWROOT//installs)
# ....
# -----------------------------------------------------
#   Running Test: ULib ...
# -----------------------------------------------------
# ULib: TROOT is EMPTY
# ULib: setup.py START - IROOT is /home/tfb/FrameworkBenchmarks/installs/pertest/ULib - TROOT is /home/tfb/FrameworkBenchmarks/ULib
# ULib: bash_profile.sh script START
# ULib: trying to start server /home/tfb/FrameworkBenchmarks/installs/pertest/ULib/ULib/bin/userver_tcp -c /home/tfb/FrameworkBenchmarks/installs/pertest/ULib/ULib/benchmark.cfg
# ULib: server STARTED
# {'results': []}
# ULib: TROOT is EMPTY
# ULib: setup.py STOP - IROOT is /home/tfb/FrameworkBenchmarks/installs/pertest/ULib - TROOT is /home/tfb/FrameworkBenchmarks/ULib
# ULib: bash_profile.sh script START
# ----------------------------------------------------------------------

# NOW THIS CALLING IS WORKING TOO BUT IT IS CORRECT TO CALL IT BEFORE INSTALLING...?
# ----------------------------------------------------------------------------------
# toolset/run-tests.py --test ULib --type all --verbose
# ----------------------------------------------------------------------------------
# ....
# ULib: setup.py running - FWROOT is /home/tfb/FrameworkBenchmarks
# ....
# -----------------------------------------------------
#   Running Test: ULib ...
# -----------------------------------------------------
# ULib: TROOT is EMPTY
# ULib: setup.py START - IROOT is /home/tfb/FrameworkBenchmarks/installs/pertest/ULib - TROOT is /home/tfb/FrameworkBenchmarks/ULib
# ULib: bash_profile.sh script START
# ULib: NOT INSTALLED...
# ULib: install.sh script START
# ULib: trying to start server /home/tfb/FrameworkBenchmarks/installs/pertest/ULib/ULib/bin/userver_tcp -c /home/tfb/FrameworkBenchmarks/installs/pertest/ULib/ULib/benchmark.cfg
# ULib: server STARTED
# {'results': []}
# ULib: TROOT is EMPTY
# ULib: setup.py STOP - IROOT is /home/tfb/FrameworkBenchmarks/installs/pertest/ULib - TROOT is /home/tfb/FrameworkBenchmarks/ULib
# ULib: bash_profile.sh script START
# -----------------------------------------------------

import os
import sys
import time
#import logging
import setup_util
import subprocess
import multiprocessing

#log = logging.getLogger('framework_test')

fwroot = setup_util.get_fwroot()

print(    "ULib: setup.py running - FWROOT is %s" % fwroot)
#log.info("ULib: setup.py running - FWROOT is %s" % fwroot)

# Queries the shell for the value of IROOT
def get_iroot():
  try:
    # Use printf to avoid getting a newline
    # Redirect to avoid stderr printing
    iroot = subprocess.check_output('printf \"$IROOT\" 2>/dev/null', shell=True, executable='/bin/bash')
    if iroot != "":
      return iroot
    print(       'ULib: IROOT is EMPTY')
#   log.critical('ULib: IROOT is EMPTY')
    return setup_util.get_fwroot() + "/installs"
  except subprocess.CalledProcessError:
    return "";

# Queries the shell for the value of TROOT
def get_troot():
  try:
    # Use printf to avoid getting a newline
    # Redirect to avoid stderr printing
    troot = subprocess.check_output('printf \"$TROOT\" 2>/dev/null', shell=True, executable='/bin/bash')
    if troot != "":
      return troot
    print(       'ULib: TROOT is EMPTY')
#   log.critical('ULib: TROOT is EMPTY')
    return setup_util.get_fwroot() + "/ULib"
  except subprocess.CalledProcessError:
    return "";

def getEnvironmentVar(name, iroot, troot):
  try:
    script = """
export IROOT=%s
. %s/bash_profile.sh 2>>/tmp/ULib_setup.txt
printf $%s           2>>/tmp/ULib_setup.txt
"""
    value = subprocess.check_output(script % (iroot, troot, name), shell=True, executable='/bin/bash')
    return value
  except subprocess.CalledProcessError:
    return "";

def callScriptAndCheckForInstallation(name, iroot, troot):
  try:
    script = """
export IROOT=%s
export TROOT=%s
. %s/%s.sh >>/tmp/ULib_setup.txt 2>&1
if [ ! -x "${ULIB_ROOT}/bin/userver_tcp" ] || [ ! -e "${ULIB_DOCUMENT_ROOT}/db.so" ] || [ ! -f "${ULIB_ROOT}/benchmark.cfg" ]; then
  exit 1
fi
"""
    print(   "ULib: %s.sh script START" % name)
#   log.info("ULib: %s.sh script START" % name)
    return subprocess.call(script % (iroot, troot, troot, name), shell=True, executable='/bin/bash')
  except subprocess.CalledProcessError:
    return 1

# --------------------------------------------------------------------------------------------------------
# TROOT - Path of this test's directory
# IROOT - Path of this test's install directory ($FWROOT/installs or $FWROOT/installs/pertest/<test-name>)
# --------------------------------------------------------------------------------------------------------
def checkEnvironment(iroot, troot):
  try:
    if callScriptAndCheckForInstallation('bash_profile', iroot, troot) != 0:
      print(       'ULib: NOT INSTALLED...')
#     log.critical('ULib: NOT INSTALLED...')
      if callScriptAndCheckForInstallation('install', iroot, troot) != 0:
        print(   "ULib: install.sh script FAILED")
#       log.info("ULib: install.sh script FAILED")
        return False
    return True
  except:
    pass
  return False

##############
# start(args)
##############
def start(args, logfile, errfile):
  try:
    iroot = get_iroot()
    troot = get_troot()

    print(   "ULib: setup.py START - IROOT is %s - TROOT is %s" % (iroot, troot))
#   log.info("ULib: setup.py START - IROOT is %s - TROOT is %s" % (iroot, troot))

    if not checkEnvironment(iroot, troot):
      return 1

    ulib_root = getEnvironmentVar('ULIB_ROOT', iroot, troot)

    fcfg = ulib_root + "/benchmark.cfg"

    # 1. Change ULib Server configuration

    # I don't understand if the two approach are different...
    #threads = str(args.max_threads)
    PROCS = str(multiprocessing.cpu_count())
    setup_util.replace_text(fcfg, "PREFORK_CHILD .*", "PREFORK_CHILD " + PROCS)

    fprg = ulib_root + "/bin/userver_tcp"

    # 2. Start ULib Server (userver_tcp)
    print(   "ULib: trying to start server " + fprg + " -c " + fcfg)
#   log.info("ULib: trying to start server " + fprg + " -c " + fcfg)

    # sudo mysqlcheck -v -r -A -u benchmarkdbuser -p
    os.putenv("ORM_DRIVER","mysql")
    os.putenv("ORM_OPTION","host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world")
    os.putenv("UMEMPOOL", "1583,1507,-19,45,16458,523,-27,-14,27")

    ulib_server_output = getEnvironmentVar('ULIB_SERVER_OUTPUT', iroot, troot)

    # Run in the background, but keep stdout/stderr for easy debugging
    subprocess.Popen(                      fprg + " -c " + fcfg + " >" + ulib_server_output + " 2>&1", shell=True, stdout=logfile, stderr=errfile)
  # subprocess.Popen("UTRACE=\"0 50M\" " + fprg + " -c " + fcfg + " >" + ulib_server_output + " 2>&1", shell=True, stdout=logfile, stderr=errfile)

    print(   "ULib: server STARTED")
#   log.info("ULib: server STARTED")
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop(logfile, errfile):
  try:
    iroot = get_iroot()
    troot = get_troot()

    print(   "ULib: setup.py STOP - IROOT is %s - TROOT is %s" % (iroot, troot))
#   log.info("ULib: setup.py STOP - IROOT is %s - TROOT is %s" % (iroot, troot))

    if not checkEnvironment(iroot, troot):
        return 1

    ulib_root = getEnvironmentVar('ULIB_ROOT', iroot, troot)

    # Stop ULib Server (userver_tcp)
    subprocess.check_call("kill -TERM $( cat " + ulib_root + "/userver_tcp.pid )", shell=True, stderr=errfile, stdout=logfile)
    time.sleep(2);
    p = subprocess.Popen(['ps', 'aux'], stdout=subprocess.PIPE)
    out, err = p.communicate()
    for line in out.splitlines():
       if 'userver_tcp' in line:
         pid = int(line.split(None, 2)[1])
         os.kill(pid, 9)
    subprocess.call("rm -f " + ulib_root + "/userver_tcp.pid /tmp/ULib_setup.txt", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
