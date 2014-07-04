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
  DOCUMENT_ROOT=$DIR/ULib/www
  if [ ! -d $DOCUMENT_ROOT ]; then
    # 1. Download ULib
    wget -nc https://github.com/stefanocasazza/ULib/archive/v1.4.0.tar.gz
    # 2. Compile application (userver_tcp)
    tar xzf v1.4.0.tar.gz
    cd ULib-1.4.0
    # ======================================================================================================
    # TO AVOID configure: error: newly created file is older than distributed files! Check your system clock
    # ======================================================================================================
    find . -exec touch {} \;
    # ======================================================================================================
    PREFIX=$DIR/installs/ulib
    if [ ! -d $PREFIX ]; then
       mkdir -p $PREFIX
    fi
    DATE=`date '+%Y%m%d'` # 20140117
    BUILD_OUTPUT=$PREFIX/ULIB_BUILD_OUTPUT-$DATE.txt
    LIBS="-lssl -lcrypto -lz" ./configure --prefix=$PREFIX --disable-static --without-libz --without-libuuid --without-magic --without-ssl --without-pcre --without-expat --with-mysql --enable-static-orm-driver=mysql --enable-static-server-plugin=http >$BUILD_OUTPUT 2>&1
    make -j1 install >>$BUILD_OUTPUT 2>&1
    cd src/ulib/net/server/plugin/usp
    make -j1 db.la fortunes.la json.la plaintext.la queries.la updates.la >>$BUILD_OUTPUT 2>&1
    if [ -e .libs/db.so ]; then
       mkdir -p $DOCUMENT_ROOT
       cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so $DOCUMENT_ROOT
    fi
    cd ~
  fi
  """

  p = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
  p.communicate(script)

  # 3. Start ULib Server (userver_tcp)
  try:
    threads = str(args.max_threads)
    setup_util.replace_text(home + "/FrameworkBenchmarks/ULib/benchmark.cfg", "PREFORK_CHILD *", "PREFORK_CHILD " + threads)

    os.putenv("ORM_DRIVER","mysql")
    os.putenv("ORM_OPTION","host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world")
    os.putenv("UMEMPOOL", "1583,1507,-19,43,1038,523,-27,-14,27")

    subprocess.check_call(home + "/FrameworkBenchmarks/installs/ulib/bin/userver_tcp -c " + home + "/FrameworkBenchmarks/ULib/benchmark.cfg", shell=True, stderr=errfile, stdout=logfile)
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
