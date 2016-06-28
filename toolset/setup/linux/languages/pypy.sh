#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/pypy.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/pypy.installed
  return 0; }
  
PYPY_ROOT=$IROOT/pypy
PYPY_VERSION=5.0.1

fw_get -o pypy-${PYPY_VERSION}-linux64.tar.bz2 https://bitbucket.org/pypy/pypy/downloads/pypy-${PYPY_VERSION}-linux64.tar.bz2
fw_untar pypy-${PYPY_VERSION}-linux64.tar.bz2
mv pypy-${PYPY_VERSION}-linux64 pypy

$PYPY_ROOT/bin/pypy -m ensurepip
$PYPY_ROOT/bin/pip install -U pip setuptools wheel

echo "export PYPY_ROOT=${PYPY_ROOT}" > $IROOT/pypy.installed
echo "export PYTHONHOME=${PYPY_ROOT}" >> $IROOT/pypy.installed
echo -e "export PATH=${PYPY_ROOT}/bin:\$PATH" >> $IROOT/pypy.installed
  
source $IROOT/pypy.installed
