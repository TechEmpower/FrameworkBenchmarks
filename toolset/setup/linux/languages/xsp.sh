#!/bin/bash

set -e

RETCODE=$(fw_exists ${IROOT}/xsp.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends mono

# mono environment variables
. ${IROOT}/mono.installed

# get
git clone git://github.com/mono/xsp
cd xsp
git checkout e272a2c006211b6b03be2ef5bbb9e3f8fefd0768

# build
./autogen.sh --prefix=${MONO_HOME} --disable-docs
make
make install

# cleanup
cd ..
rm -rf xsp

touch ${IROOT}/xsp.installed
