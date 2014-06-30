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
  if [ ! -d ~/FrameworkBenchmarks/ULib/www ]; then
    # 1. Download ULib
    sudo wget https://github.com/stefanocasazza/ULib/archive/master.zip 
    # 2. Compile application (userver_tcp)
    sudo unzip master.zip
    cd ULib-master
    sudo LIBS="-lssl -lcrypto -lz" ./configure --disable-static --disable-log --without-libz --without-libuuid --without-magic --without-ssl --without-pcre --without-expat --with-mysql --enable-static-orm-driver=mysql --enable-static-server-plugin=http >/dev/null 2>&1
    sudo make install >/dev/null 2>&1
    cd src/ulib/net/server/plugin/usp
    sudo make db.la fortunes.la json.la plaintext.la queries.la updates.la >/dev/null 2>&1
    sudo mkdir -p ~/FrameworkBenchmarks/ULib/www
    sudo cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so ~/FrameworkBenchmarks/ULib/www
    cd ~
  fi
  """

  p = subprocess.Popen(['sh'], stdin=subprocess.PIPE)
  p.communicate(script)

  # 3. Start ULib Server
  try:
    threads = str(args.max_threads)
    setup_util.replace_text(home + "/FrameworkBenchmarks/ULib/benchmark.cfg", "PREFORK_CHILD *", "PREFORK_CHILD " + threads)

    os.putenv("ORM_DRIVER","mysql")
    os.putenv("ORM_OPTION","host=" + args.database_host + " user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world")
    os.putenv("UMEMPOOL", "1583,1507,-19,43,1038,523,-27,-14,27")

    subprocess.check_call("sudo /usr/local/bin/userver_tcp -c " + home + "/FrameworkBenchmarks/ULib/benchmark.cfg", shell=True, stderr=errfile, stdout=logfile)
    return 0
  except subprocess.CalledProcessError:
    return 1

##############
# stop()
##############
def stop(logfile, errfile):
  try:
    subprocess.check_call("sudo kill -TERM $( cat " + home + "/FrameworkBenchmarks/ULib/userver_tcp.pid )", shell=True, stderr=errfile, stdout=logfile)
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
