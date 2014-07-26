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

if [ ! -d "$ULIB_ROOT" ]; then
  mkdir -p $ULIB_ROOT
fi

cd $ULIB_ROOT

# 1. Download ULib
if [ ! -f "v${ULIB_VERSION}.tar.gz" ]; then
	wget -nc --no-check-certificate --trust-server-names -O v${ULIB_VERSION}.tar.gz \
        https://github.com/stefanocasazza/ULib/archive/v${ULIB_VERSION}.tar.gz 2>/dev/null

	tar xf v${ULIB_VERSION}.tar.gz 2>/dev/null
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
   exit 1
fi

mkdir -p $ULIB_DOCUMENT_ROOT
cp .libs/db.so .libs/fortunes.so .libs/json.so .libs/plaintext.so .libs/queries.so .libs/updates.so $ULIB_DOCUMENT_ROOT
