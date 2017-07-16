#!/bin/bash

fw_installed pypy3 && return 0
  
PYPY3_ROOT=$IROOT/pypy3
PYPY3_VERSION=5.8.0

fw_get -o pypy3-v${PYPY_VERSION}-linux64.tar.bz2 https://bitbucket.org/pypy/pypy/downloads/pypy3-v${PYPY_VERSION}-linux64.tar.bz2
fw_untar pypy3-v${PYPY_VERSION}-linux64.tar.bz2
mv pypy3-v${PYPY_VERSION}-linux64 pypy3

$PYPY_ROOT/bin/pypy3 -m ensurepip
$PYPY_ROOT/bin/pip install -U pip setuptools wheel

echo "export PYPY3_ROOT=${PYPY3_ROOT}" > $IROOT/pypy3.installed
echo "export PYTHONHOME=${PYPY3_ROOT}" >> $IROOT/pypy3.installed
echo -e "export PATH=${PYPY3_ROOT}/bin:\$PATH" >> $IROOT/pypy3.installed
  
source $IROOT/pypy3.installed
