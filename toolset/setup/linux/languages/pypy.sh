#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/pypy.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/pypy.installed
  return 0; }
  
PYPY_ROOT=$IROOT/pypy

fw_get -o pypy-2.5.0-linux64.tar.bz2 https://bitbucket.org/pypy/pypy/downloads/pypy-2.5.0-linux64.tar.bz2
fw_untar pypy-2.5.0-linux64.tar.bz2
ln -sf pypy-2.5.0-linux64 pypy

fw_get -o get-pip.py https://bootstrap.pypa.io/get-pip.py

# Ensure pip is installed
$PYPY_ROOT/bin/pypy get-pip.py

echo "export PYPY_ROOT=${PYPY_ROOT}" > $IROOT/pypy.installed
echo "export PYTHONHOME=${PYPY_ROOT}" >> $IROOT/pypy.installed
echo -e "export PATH=${PYPY_ROOT}/bin:\$PATH" >> $IROOT/pypy.installed
  
source $IROOT/pypy.installed
