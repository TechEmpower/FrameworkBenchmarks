#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/py2.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://www.python.org/ftp/python/2.7.9/Python-2.7.9.tgz
fw_untar Python-2.7.9.tgz
cd Python-2.7.9
./configure --prefix=${IROOT}/py2 --disable-shared --quiet
make -j4 --quiet
make install --quiet

${IROOT}/py2/bin/python -m ensurepip -U
${IROOT}/py2/bin/pip install -U setuptools pip

touch ${IROOT}/py2.installed
