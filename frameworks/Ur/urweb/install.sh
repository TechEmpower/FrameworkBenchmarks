#!/bin/bash

VERSION=20141206
COMPILER=/usr/local/bin/urweb

RETCODE=$(fw_exists $COMPILER)
[ ! "$RETCODE" == 0 ] || [ ! `$COMPILER | grep -oE '[^ ]+$'` == "$VERSION" ] || { return 0; }

fw_get http://www.impredicative.com/ur/urweb-$VERSION.tgz
fw_untar urweb-$VERSION.tgz
cd urweb-$VERSION
./configure
make
sudo make install
