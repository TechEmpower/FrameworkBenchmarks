#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/urweb.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.impredicative.com/ur/urweb-20140830.tgz
fw_untar urweb-20140830.tgz
cd urweb-20140830
./configure --prefix=${IROOT}/urweb
make
make install

touch ${IROOT}/urweb.installed