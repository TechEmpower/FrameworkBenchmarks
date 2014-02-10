import subprocess
import sys
import os
import setup_util

from os.path import expanduser

home = expanduser("~")

##############
# start(args)
##############
def start(args, logfile, errfile):

  # 1. Download ULib
  # 2. Compile applicaton

  script = """
  ##############################
  # Prerequisites
  ##############################
  # yes | sudo apt-get update
  # yes | sudo apt-get install ... 
  ##############################
  # ULib
  ##############################
  git clone https://github.com/stefanocasazza/ULib.git
  cd ULib
  ./configure --disable-static --disable-log --without-libz --without-libuuid --without-magic --without-ssl --without-pcre --without-expat --with-mysql --enable-static-orm-driver=mysql --enable-static-server-plugin=http >/dev/null 2>&1
  make install
  cd src/ulib/net/server/plugin/usp
  make install >/dev/null 2>&1
  make db.la fortunes.la json.la plaintext.la queries.la updates.la >/dev/null 2>&1
  sudo cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so ~/FrameworkBenchmarks/ULib/www
  cd ~
  """

  p = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
  p.communicate(script)

  # 3. Start ULib Server
  try:
    threads = str(args.max_threads)
    setup_util.replace_text(home + "/FrameworkBenchmarks/ULib/benchmark.cfg", "PREFORK_CHILD *", "PREFORK_CHILD " + threads)

    os.putenv("ORM_DRIVER","mysql")
    os.putenv("ORM_OPTION","host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world")
    os.putenv("UMEMPOOL", "1583,1507,-19,34,1033,523,-27,-14,27")

    subprocess.check_call("/usr/local/bin/userver_tcp -c " + home + "/FrameworkBenchmarks/ULib/benchmark.cfg", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop(logfile, errfile):
  try:
    subprocess.check_call("sudo kill -TERM $( cat " + home + "/FrameworkBenchmarks/ULib/userver_tcp.pid )", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1
