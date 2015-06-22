#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/pypy.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get https://bitbucket.org/pypy/pypy/downloads/pypy-2.6.0-linux64.tar.bz2 -o pypy-2.6.0-linux64.tar.bz2
fw_untar pypy-2.6.0-linux64.tar.bz2
ln -sf pypy-2.6.0-linux64 pypy

${IROOT}/pypy/bin/pypy -m ensurepip -U
${IROOT}/pypy/bin/pip install -U setuptools pip

touch ${IROOT}/pypy.installed
