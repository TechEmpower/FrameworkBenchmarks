#!/bin/bash

#----------------------------------------------------------------------------------------------------
# toolset/run-tests.py --install server --install-strategy pertest --name ULib --test ULib --type all
#----------------------------------------------------------------------------------------------------

source ${HOME}/FrameworkBenchmarks/ULib/bash_profile.sh

cat <<EOF >${ULIB_ROOT}/benchmark.cfg
userver {
 PORT 8080
 PREFORK_CHILD 8
 LISTEN_BACKLOG 16384
 MAX_KEEP_ALIVE 16384
 DOCUMENT_ROOT ${ULIB_DOCUMENT_ROOT}
 PID_FILE ${ULIB_ROOT}/userver_tcp.pid
}
EOF

RET1=$(fw_exists ${ULIB_ROOT}/bin/userver_tcp)
RET2=$(fw_exists ${ULIB_DOCUMENT_ROOT}/db.so)

if [ "$RET1" == 0 ] && [ "$RET2" == 0 ]; then
  return 0;
fi

# 1. Download ULib
wget -nc --no-check-certificate --trust-server-names -O v${ULIB_VERSION}.tar.gz https://github.com/stefanocasazza/ULib/archive/v${ULIB_VERSION}.tar.gz

# 2. Compile application (userver_tcp)
fw_untar v${ULIB_VERSION}.tar.gz

cd ULib-$ULIB_VERSION
# ======================================================================================================
# TO AVOID configure: error: newly created file is older than distributed files! Check your system clock
# ======================================================================================================
find . -exec touch {} \;
# ======================================================================================================

LIBS="-lssl -lcrypto -lz" \
./configure --prefix=$ULIB_ROOT \
            --disable-static \
            --with-mysql \
            --without-ssl --without-pcre --without-expat \
            --without-libz --without-libuuid --without-magic \
            --enable-static-orm-driver=mysql --enable-static-server-plugin=http >$ULIB_BUILD_OUTPUT 2>&1

make -j1 install >>$ULIB_BUILD_OUTPUT 2>&1

# 3. Compile usp pages for benchmark
cd src/ulib/net/server/plugin/usp
make -j1 db.la fortunes.la json.la plaintext.la queries.la updates.la >>$ULIB_BUILD_OUTPUT 2>&1

if [ ! -e .libs/db.so ]; then
   return 1;
fi

mkdir -p $ULIB_DOCUMENT_ROOT
cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so $ULIB_DOCUMENT_ROOT
