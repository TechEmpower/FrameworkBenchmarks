#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/xsp.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends mono

. mono-snapshot mono/20141222114925

git clone git://github.com/mono/xsp
cd xsp
git checkout e272a2c006211b6b03be2ef5bbb9e3f8fefd0768

./autogen.sh --prefix=${MONO_PREFIX}
make
sudo make install

touch ${IROOT}/xsp.installed
