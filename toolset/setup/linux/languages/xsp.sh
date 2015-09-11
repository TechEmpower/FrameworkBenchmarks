#!/bin/bash

fw_depends mono

RETCODE=$(fw_exists ${IROOT}/xsp.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/xsp.installed
  return 0; }

# get
git clone git://github.com/mono/xsp
cd xsp
git checkout e272a2c006211b6b03be2ef5bbb9e3f8fefd0768

# build
./autogen.sh --prefix=$MONO_HOME --disable-docs
make
make install

# cleanup
cd ..
rm -rf xsp

echo "" > $IROOT/xsp.installed
