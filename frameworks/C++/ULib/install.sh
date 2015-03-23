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

ULIB_VERSION=1.4.2
ULIB_ROOT=$IROOT/ULib
ULIB_DOCUMENT_ROOT=${ULIB_ROOT}/ULIB_DOCUMENT_ROOT

# Check if ULib is already installed
ULIB_INSTALLED_FILE="${IROOT}/ULib-${ULIB_VERSION}.installed"
RETCODE=$(fw_exists ${ULIB_INSTALLED_FILE})
[ ! "$RETCODE" == 0 ] || { return 0; }

# Create a run directory for ULIB
[ ! -e ${ULIB_INSTALLED_FILE} -a -d ${IROOT}/ULib ] && rm -rf ${IROOT}/ULib*

if [ ! -d "$ULIB_ROOT" ]; then
  mkdir -p $ULIB_ROOT
fi

# AVOID "fatal error: postgres_fe.h: No such file or directory"
sudo apt-get install -y postgresql-server-dev-all

# Add a simple configuration file to it
cd $ULIB_ROOT
if [ ! -f "benchmark.cfg" ]; then
  cat <<EOF >benchmark.cfg
userver {
 PORT 8080
 PREFORK_CHILD 4
 MAX_KEEP_ALIVE 1023
 LISTEN_BACKLOG 16384
 CLIENT_FOR_PARALLELIZATION 256
 ORM_DRIVER "mysql pgsql sqlite"
 DOCUMENT_ROOT $ULIB_DOCUMENT_ROOT
}
EOF
fi

# 1. Download ULib
cd $IROOT
fw_get -O ULib-${ULIB_VERSION}.tar.gz https://github.com/stefanocasazza/ULib/archive/v${ULIB_VERSION}.tar.gz 
fw_untar  ULib-${ULIB_VERSION}.tar.gz

# 2. Compile application (userver_tcp)
cd ULib-$ULIB_VERSION

# Check for the compiler support (We want at least g++ 4.8)
CC=gcc  # C   compiler command
CXX=g++ # C++ compiler command

gcc_version=`g++ -dumpversion`

case "$gcc_version" in
  3*|4.0*|4.1*|4.2*|4.3*|4.4*|4.5*|4.6*|4.7*)
	  CC='gcc-4.8'
	 CXX='g++-4.8'
  ;;
esac

export CC CXX

# AVOID "configure: error: newly created file is older than distributed files! Check your system clock"
find . -exec touch {} \;

USP_FLAGS="-DAS_cpoll_cppsp_DO" \
./configure --prefix=$ULIB_ROOT \
   --disable-static --disable-examples \
   --with-mysql --with-pgsql --with-sqlite3 \
   --without-ssl --without-pcre --without-expat \
   --without-libz --without-libuuid --without-magic --without-libares \
   --enable-static-orm-driver='mysql pgsql sqlite' --enable-static-server-plugin=http
#  --enable-debug \
#USP_LIBS="-ljson" \

make install
cp -r tests/examples/benchmark/FrameworkBenchmarks/ULib/db ${ULIB_ROOT}

cd examples/userver
make install

# 3. Compile usp pages for benchmark
cd ../../src/ulib/net/server/plugin/usp
make db.la fortune.la json.la plaintext.la query.la update.la

# Check that compilation worked
if [ ! -e .libs/db.so ]; then
   exit 1
fi

mkdir -p $ULIB_DOCUMENT_ROOT
cp .libs/db.so .libs/fortune.so .libs/json.so .libs/plaintext.so .libs/query.so .libs/update.so $ULIB_DOCUMENT_ROOT

touch ${ULIB_INSTALLED_FILE}
