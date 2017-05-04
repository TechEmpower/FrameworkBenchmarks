#!/bin/bash

fw_installed pypy && return 0
  
PYPY_ROOT=$IROOT/pypy
PYPY_VERSION=5.7.1

fw_get -o pypy2-v${PYPY_VERSION}-linux64.tar.bz2 https://bitbucket.org/pypy/pypy/downloads/pypy2-v${PYPY_VERSION}-linux64.tar.bz2
fw_untar pypy2-v${PYPY_VERSION}-linux64.tar.bz2
mv pypy2-v${PYPY_VERSION}-linux64 pypy

$PYPY_ROOT/bin/pypy -m ensurepip
$PYPY_ROOT/bin/pip install -U pip setuptools wheel

echo "export PYPY_ROOT=${PYPY_ROOT}" > $IROOT/pypy.installed
echo "export PYTHONHOME=${PYPY_ROOT}" >> $IROOT/pypy.installed
echo -e "export PATH=${PYPY_ROOT}/bin:\$PATH" >> $IROOT/pypy.installed
  
source $IROOT/pypy.installed
