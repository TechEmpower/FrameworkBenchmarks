#!/bin/bash

PY2_ROOT=$IROOT/py2
RETCODE=$(fw_exists ${PY2_ROOT}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $PY2_ROOT.installed
  return 0; }

fw_get -O http://www.python.org/ftp/python/2.7.10/Python-2.7.10.tgz
fw_untar Python-2.7.10.tgz
cd Python-2.7.10
./configure --prefix=${IROOT}/py2 --disable-shared --quiet
make -j4 --quiet 2>&1 | tee $IROOT/python-install.log | awk '{ if (NR%100 == 0) printf "."}'
make install --quiet 2>&1 | tee -a $IROOT/python-install.log | awk '{ if (NR%100 == 0) printf "."}'
cd ..

$PY2_ROOT/bin/python -m ensurepip -U
$PY2_ROOT/bin/pip install -U setuptools pip

echo "export PY2_ROOT=${PY2_ROOT}" > $PY2_ROOT.installed
echo "export PYTHONHOME=${PY2_ROOT}" >> $PY2_ROOT.installed
echo -e "export PATH=${PY2_ROOT}/bin:\$PATH" >> $PY2_ROOT.installed

source $PY2_ROOT.installed
