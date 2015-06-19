#!/bin/bash

PYPY_ROOT=$IROOT/pypy
RETCODE=$(fw_exists ${PYPY_ROOT}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $PYPY_ROOT.installed
  return 0; }

fw_get https://bitbucket.org/pypy/pypy/downloads/pypy-2.5.0-linux64.tar.bz2 -o pypy-2.5.0-linux64.tar.bz2
fw_untar pypy-2.5.0-linux64.tar.bz2
ln -sf pypy-2.5.0-linux64 pypy

fw_get https://bootstrap.pypa.io/get-pip.py -o get-pip.py

# Ensure pip is installed
$PYPY_ROOT/bin/pypy get-pip.py

echo "export PYPY_ROOT=${PYPY_ROOT}" > $PYPY_ROOT.installed
echo "export PYTHONHOME=${PYPY_ROOT}" >> $PYPY_ROOT.installed
echo -e "export PATH=${PYPY_ROOT}/bin:\$PATH" >> $PYPY_ROOT.installed
  
source $PYPY_ROOT.installed
