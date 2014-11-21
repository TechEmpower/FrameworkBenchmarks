#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/py2.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/2.7.8/Python-2.7.8.tgz
fw_untar Python-2.7.8.tgz
cd Python-2.7.8
./configure --prefix=${IROOT}/py2 --disable-shared --quiet
make -j4 --quiet
make install --quiet

cd ..
if [ ! -f "get-pip.py" ]; then
fw_get https://bootstrap.pypa.io/get-pip.py -O get-pip.py
fi
${IROOT}/py2/bin/python get-pip.py

touch ${IROOT}/py2.installed