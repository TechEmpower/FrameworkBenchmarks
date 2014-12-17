#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/py3.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/3.4.2/Python-3.4.2.tar.xz
fw_untar Python-3.4.2.tar.xz
cd Python-3.4.2
./configure --prefix=${IROOT}/py3 --disable-shared --quiet
make -j4 --quiet
make install --quiet

touch ${IROOT}/py3.installed