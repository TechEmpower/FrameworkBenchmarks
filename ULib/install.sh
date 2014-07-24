#!/bin/bash

# ---------------------------------------------------------------------------------------------------------------------------------
# toolset/run-tests.py --install server --install-strategy pertest --install-error-action continue --test ULib --type all --verbose
# ---------------------------------------------------------------------------------------------------------------------------------
# INFO:root:Running installation for ULib
# INSTALL: 
#    export TROOT=$FWROOT/ULib && 
#    export IROOT=$FWROOT/installs && 
#    . $FWROOT/toolset/setup/linux/bash_functions.sh && 
#    . $FWROOT/ULib/install.sh (cwd=$FWROOT//installs)
# ---------------------------------------------------------------------------------------------------------------------------------

if [ -z "$TROOT" ]; then
   return 1
fi

. ${TROOT}/bash_profile.sh

if [ ! -d "$ULIB_ROOT" ]; then
  mkdir -p $ULIB_ROOT
fi

cd ${ULIB_ROOT}

if [ ! -f "benchmark.cfg" ]; then
  cat <<EOF >benchmark.cfg
userver {
 PORT 8080
 PREFORK_CHILD 8
 LISTEN_BACKLOG 8192
 MAX_KEEP_ALIVE 8192
 DOCUMENT_ROOT ${ULIB_DOCUMENT_ROOT}
 PID_FILE ${ULIB_ROOT}/userver_tcp.pid
}
EOF
fi

if [ -x "bin/userver_tcp" ] && [ -x "${ULIB_DOCUMENT_ROOT}/db.so" ]; then
  return 0
fi

# 1. Download ULib
if [ ! -f "v${ULIB_VERSION}.tar.gz" ]; then
	wget -nc --no-check-certificate --trust-server-names -O v${ULIB_VERSION}.tar.gz \
        https://github.com/stefanocasazza/ULib/archive/v${ULIB_VERSION}.tar.gz 2>/dev/null

	tar xvf v${ULIB_VERSION}.tar.gz 2>/dev/null
fi

cd ULib-$ULIB_VERSION

# 2. Compile application (userver_tcp)
# ======================================================================================================
# TO AVOID configure: error: newly created file is older than distributed files! Check your system clock
# ======================================================================================================
find . -exec touch {} \;
# ======================================================================================================

#           --enable-debug \
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
   return 1
fi

mkdir -p $ULIB_DOCUMENT_ROOT
cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so $ULIB_DOCUMENT_ROOT
