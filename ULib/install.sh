#!/bin/bash

# FWROOT=${HOME}/FrameworkBenchmarks
#  IROOT=${FWROOT}/ulib/installs

DOCUMENT_ROOT=$FWROOT/ULib/www

RET1=$(fw_exists $IROOT/bin/userver_tcp)
RET2=$(fw_exists $DOCUMENT_ROOT/db.so)

if [ "$RET1" == 0 ] && [ "$RET2" == 0 ]; then
  return 0;
fi

VERSION=1.4.1
# 1. Download ULib
fw_get https://github.com/stefanocasazza/ULib/archive/v$VERSION.tar.gz
# 2. Compile application (userver_tcp)
fw_untar v$VERSION.tar.gz
cd ULib-$VERSION
# ======================================================================================================
# TO AVOID configure: error: newly created file is older than distributed files! Check your system clock
# ======================================================================================================
find . -exec touch {} \;
# ======================================================================================================
if [ ! -d $IROOT ]; then
   mkdir -p $IROOT
fi
DATE=`date '+%Y%m%d'` # 20140117
BUILD_OUTPUT=$IROOT/ULIB_BUILD_OUTPUT-$DATE.txt
LIBS="-lssl -lcrypto -lz" ./configure --prefix=$IROOT --disable-static --without-libz --without-libuuid --without-magic --without-ssl --without-pcre --without-expat --with-mysql --enable-static-orm-driver=mysql --enable-static-server-plugin=http >$BUILD_OUTPUT 2>&1
make -j1 install >>$BUILD_OUTPUT 2>&1
cd src/ulib/net/server/plugin/usp
make -j1 db.la fortunes.la json.la plaintext.la queries.la updates.la >>$BUILD_OUTPUT 2>&1
if [ -e .libs/db.so ]; then
   mkdir -p $DOCUMENT_ROOT
   cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so $DOCUMENT_ROOT
else
	return 1;
fi
if [ ! -f $FWROOT/ULib/benchmark.cfg ]; then
  cat <<EOF >$FWROOT/ULib/benchmark.cfg
userver {
  PORT 8080
  PREFORK_CHILD 8
  LISTEN_BACKLOG 16384
  MAX_KEEP_ALIVE 16384
  DOCUMENT_ROOT ~/FrameworkBenchmarks/ULib/www
  PID_FILE ~/FrameworkBenchmarks/ULib/userver_tcp.pid
}
EOF
