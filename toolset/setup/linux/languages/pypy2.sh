#!/bin/bash

fw_installed pypy2 && return 0
  
PYPY2_ROOT=$IROOT/pypy2
PYPY2_VERSION=5.8.0

fw_get -o pypy2-v${PYPY2_VERSION}-linux64.tar.bz2 https://bitbucket.org/pypy/pypy/downloads/pypy2-v${PYPY2_VERSION}-linux64.tar.bz2
fw_untar pypy2-v${PYPY2_VERSION}-linux64.tar.bz2
mv pypy2-v${PYPY2_VERSION}-linux64 pypy2

$PYPY2_ROOT/bin/pypy -m ensurepip
$PYPY2_ROOT/bin/pip install -U pip setuptools wheel

echo "export PYPY2_ROOT=${PYPY2_ROOT}" > $IROOT/pypy2.installed
echo "export PYTHONHOME=${PYPY2_ROOT}" >> $IROOT/pypy2.installed
echo -e "export PATH=${PYPY2_ROOT}/bin:\$PATH" >> $IROOT/pypy2.installed
  
source $IROOT/pypy2.installed
