#!/bin/bash

# install.sh
# --------------------------------------------------------------------------------------------------------
# toolset/run-tests.py --install server --test ULib-mysql  --type all --verbose
# toolset/run-tests.py --install server --test ULib-sqlite --type all --verbose
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

# Check if ULib is already installed
ULIB_INSTALLED_FILE="${IROOT}/ULib-${ULIB_VERSION}.installed"
RETCODE=$(fw_exists ${ULIB_INSTALLED_FILE})
[ ! "$RETCODE" == 0 ] || { return 0; }

# ULib is only built during installation as a dependency sanity check
#sudo apt-get update
 sudo apt-get install libmysqlclient-dev libsqlite3-dev

# Create a run directory for ULIB
[ ! -e ${ULIB_INSTALLED_FILE} -a -d ${IROOT}/ULib ] && rm -rf ${IROOT}/ULib*

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
fw_untar  ULib-${ULIB_VERSION}.tar.gz

# 2. Compile application (userver_tcp)

cd ULib-$ULIB_VERSION

# AVOID "configure: error: newly created file is older than distributed files! Check your system clock"
find . -exec touch {} \;

./configure --prefix=$ULIB_ROOT \
            --disable-static \
            --with-mysql --with-sqlite3 \
            --without-ssl --without-pcre --without-expat \
            --without-libz --without-libuuid --without-magic \
            --enable-static-orm-driver='mysql sqlite' --enable-static-server-plugin=http
#           --enable-debug \
make install

# 3. Compile usp pages for benchmark
cd src/ulib/net/server/plugin/usp
make db.la fortune.la json.la plaintext.la query.la update.la

# Check that compilation worked
if [ ! -e .libs/db.so ]; then
   exit 1
fi

mkdir -p $ULIB_DOCUMENT_ROOT
cp .libs/db.so .libs/fortune.so .libs/json.so .libs/plaintext.so .libs/query.so .libs/update.so $ULIB_DOCUMENT_ROOT

cd $IROOT
cp -r ULib-1.4.2/tests/examples/benchmark/FrameworkBenchmarks/ULib/db $ULIB_ROOT

touch ${ULIB_INSTALLED_FILE}
