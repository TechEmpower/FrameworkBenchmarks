#!/bin/bash

#------------------------------------------------------------ -------------------------------------------------------
# toolset/run-tests.py --install server --install-strategy pertest --max-threads 1 --name ULib --test ULib --type all
#------------------------------------------------------------ -------------------------------------------------------
# IROOT - Path of this test's install directory ($FWROOT/installs or $FWROOT/installs/pertest/<test-name>)
# TROOT - Path of this test's directory
#------------------------------------------------------------ -------------------------------------------------------

DOCUMENT_ROOT=${TROOT}/www

RET1=$(fw_exists ${IROOT}/bin/userver_tcp)
RET2=$(fw_exists ${DOCUMENT_ROOT}/db.so)

if [ "$RET1" == 0 ] && [ "$RET2" == 0 ]; then
  return 0;
fi

VERSION=1.4.1
# 1. Download ULib
#fw_get                                              "https://github.com/stefanocasazza/ULib/archive/v${VERSION}.tar.gz"
wget -nc --no-check-certificate --trust-server-names "https://github.com/stefanocasazza/ULib/archive/v${VERSION}.tar.gz"

# 1a. Check for Location: https://codeload.github.com/stefanocasazza/ULib/tar.gz/v1.4.1
if [ -e v${VERSION} ]; then
   mv v${VERSION} v${VERSION}.tar.gz
fi

# 2. Compile application (userver_tcp)
fw_untar v${VERSION}.tar.gz

cd ULib-$VERSION
# ======================================================================================================
# TO AVOID configure: error: newly created file is older than distributed files! Check your system clock
# ======================================================================================================
find . -exec touch {} \;
# ======================================================================================================

BUILD_OUTPUT=${IROOT}/ULIB_BUILD_OUTPUT.txt

LIBS="-lssl -lcrypto -lz" \
./configure --prefix=$IROOT \
				--disable-static \
				--with-mysql \
				--without-ssl --without-pcre --without-expat \
				--without-libz --without-libuuid --without-magic \
				--enable-static-orm-driver=mysql --enable-static-server-plugin=http >$BUILD_OUTPUT 2>&1

make -j1 install >>$BUILD_OUTPUT 2>&1

# 3. Compile usp pages for benchmark
cd src/ulib/net/server/plugin/usp
make -j1 db.la fortunes.la json.la plaintext.la queries.la updates.la >>$BUILD_OUTPUT 2>&1

if [ ! -e .libs/db.so ]; then
	return 1;
fi

mkdir -p $DOCUMENT_ROOT
cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so $DOCUMENT_ROOT

cat <<EOF >${TROOT}/benchmark.cfg
userver {
 PORT 8080
 PREFORK_CHILD 8
 LISTEN_BACKLOG 16384
 MAX_KEEP_ALIVE 16384
 DOCUMENT_ROOT ${DOCUMENT_ROOT}
 PID_FILE ${IROOT}/userver_tcp.pid
}
EOF
