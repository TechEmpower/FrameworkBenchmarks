#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/py3.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/3.4.2/Python-3.4.2.tar.xz
fw_untar Python-3.4.2.tar.xz
cd Python-3.4.2
./configure --prefix=${IROOT}/py3 --disable-shared --with-computed-gotos --quiet
make -j4 --quiet 2>&1 | tee $IROOT/python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
make install --quiet 2>&1 | tee -a $IROOT/python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
cd ..

ln -s ${IROOT}/py3/bin/python3.4m ${IROOT}/py3/bin/python3.4
${IROOT}/py3/bin/python3 -m ensurepip -U
${IROOT}/py3/bin/pip3 install -U setuptools pip

touch ${IROOT}/py3.installed
