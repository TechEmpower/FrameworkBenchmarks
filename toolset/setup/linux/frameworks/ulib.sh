#!/bin/bash


RETCODE=$(fw_exists ${IROOT}/ulib.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/ulib.installed
  return 0; }

ULIB_VERSION=1.4.2
ULIB_ROOT=$IROOT/ULib
ULIB_DOCUMENT_ROOT=$ULIB_ROOT/ULIB_DOCUMENT_ROOT

# Create a run directory for ULIB
[ ! -e $IROOT/ulib.installed -a -d $IROOT/ULib ] && rm -rf $IROOT/ULib*

if [ ! -d "$ULIB_ROOT" ]; then
  mkdir -p $ULIB_ROOT
fi

# AVOID "fatal error: postgres_fe.h: No such file or directory"
# TODO: This should already be installed and unnecessary.
sudo apt-get install -y postgresql-server-dev-all

# make use of FIFO scheduling policy possible
type setcap >/dev/null 2>/dev/null

if [ $? -ne 0 ]; then
   sudo apt-get install -y libcap2-bin
fi

# Add a simple configuration file to it
cd $ULIB_ROOT
if [ ! -f "benchmark.cfg" ]; then
  cat <<EOF >benchmark.cfg
userver {
 PORT 8080
 PREFORK_CHILD 4
 TCP_LINGER_SET -1
 LISTEN_BACKLOG 256
 ORM_DRIVER "mysql pgsql sqlite"
 DOCUMENT_ROOT $ULIB_DOCUMENT_ROOT
}
EOF
fi

# 1. Download ULib
cd $IROOT
fw_get -o ULib-${ULIB_VERSION}.tar.gz https://github.com/stefanocasazza/ULib/archive/v${ULIB_VERSION}.tar.gz 
fw_untar  ULib-${ULIB_VERSION}.tar.gz

# 2. Compile application (userver_tcp)
cd ULib-$ULIB_VERSION

# Check for the compiler support (We want at least g++ 4.8)
CC=gcc  # C   compiler command
CXX=g++ # C++ compiler command

gcc_version=`g++ -dumpversion`

case "$gcc_version" in
  3*|4.0*|4.1*|4.2*|4.3*|4.4*|4.5*|4.6*|4.7*)
	  CC='gcc-4.9'
	 CXX='g++-4.9'
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
cp -r tests/examples/benchmark/FrameworkBenchmarks/ULib/db $ULIB_ROOT

cd examples/userver
make install

# 3. Compile usp pages for benchmark
cd ../../src/ulib/net/server/plugin/usp
make db.la fortune.la json.la plaintext.la query.la update.la rdb.la rquery.la rupdate.la rfortune.la

# Check that compilation worked
if [ ! -e .libs/db.so ]; then
   exit 1
fi

mkdir -p $ULIB_DOCUMENT_ROOT
cp .libs/db.so .libs/fortune.so .libs/json.so .libs/plaintext.so .libs/query.so .libs/update.so .libs/rdb.so .libs/rquery.so .libs/rupdate.so .libs/rfortune.so $ULIB_DOCUMENT_ROOT

echo "export ULIB_VERSION=${ULIB_VERSION}" >> $IROOT/ulib.installed
echo "export ULIB_ROOT=${ULIB_ROOT}" >> $IROOT/ulib.installed
echo "export ULIB_DOCUMENT_ROOT=${ULIB_DOCUMENT_ROOT}" >> $IROOT/ulib.installed
echo -e "export PATH=\$ULIB_ROOT/bin:\$PATH" >> $IROOT/ulib.installed

source $IROOT/ulib.installed
