#!/bin/bash

# install.sh
# --------------------------------------------------------------------------------------------------------
# toolset/run-tests.py --install server --test ULib --type all --verbose
# --------------------------------------------------------------------------------------------------------
# TROOT - Path of this test's directory
# IROOT - Path of this test's install directory ($FWROOT/installs or $FWROOT/installs/pertest/<test-name>)
# --------------------------------------------------------------------------------------------------------
# INFO:root:Running installation for ULib
# INSTALL: 
#    export TROOT=$FWROOT/ULib && 
#    export IROOT=$FWROOT/installs && 
#    . $FWROOT/toolset/setup/linux/bash_functions.sh && 
#    . $FWROOT/ULib/install.sh (cwd=$FWROOT//installs)
# --------------------------------------------------------------------------------------------------------

# Chekc if ULib is already installed
RETCODE=$(fw_exists ulib-${ULIB_VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

# Create a run directory for ULIB
if [ ! -d "$ULIB_ROOT" ]; then
  mkdir -p $ULIB_ROOT
fi

# Add a simple configuration file to it
cd $ULIB_ROOT
if [ ! -f "benchmark.cfg" ]; then
  cat <<EOF >benchmark.cfg
userver {
 PORT 8080
 PREFORK_CHILD 8
 LISTEN_BACKLOG 8192
 MAX_KEEP_ALIVE 8192
 DOCUMENT_ROOT $ULIB_DOCUMENT_ROOT
 PID_FILE ${ULIB_ROOT}/userver_tcp.pid
}
EOF
fi

# 1. Download ULib
cd $IROOT
fw_get -O ULib-${ULIB_VERSION}.tar.gz https://github.com/stefanocasazza/ULib/archive/v${ULIB_VERSION}.tar.gz 
fw_untar ULib-${ULIB_VERSION}.tar.gz

# 2. Compile application (userver_tcp)

cd ULib-$ULIB_VERSION

# AVOID "configure: error: newly created file is older than distributed files! Check your system clock"
find . -exec touch {} \;

LIBS="-lssl -lcrypto -lz" \
./configure --prefix=$ULIB_ROOT \
            --disable-static \
            --with-mysql \
            --without-ssl --without-pcre --without-expat \
            --without-libz --without-libuuid --without-magic \
            --enable-static-orm-driver=mysql --enable-static-server-plugin=http
#           --enable-debug \
make install

# 3. Compile usp pages for benchmark
cd src/ulib/net/server/plugin/usp
make db.la fortunes.la json.la plaintext.la queries.la updates.la

# Check that compilation worked
if [ ! -e .libs/db.so ]; then
   exit 1
fi

mkdir -p $ULIB_DOCUMENT_ROOT
cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so $ULIB_DOCUMENT_ROOT

cd $IROOT
touch ulib-${ULIB_VERSION}.installed 
