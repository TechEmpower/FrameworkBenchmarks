#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/py2.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/py2.installed
  return 0; }

PY2_ROOT=$IROOT/py2
PY2_VERSION=2.7.11

fw_get -O http://www.python.org/ftp/python/${PY2_VERSION}/Python-${PY2_VERSION}.tar.xz
fw_untar Python-${PY2_VERSION}.tar.xz
cd Python-${PY2_VERSION}
./configure --prefix=${IROOT}/py2 --disable-shared --quiet
make -j4 --quiet 2>&1 | tee $IROOT/python-install.log | awk '{ if (NR%100 == 0) printf "."}'
make install --quiet 2>&1 | tee -a $IROOT/python-install.log | awk '{ if (NR%100 == 0) printf "."}'
cd ..

$PY2_ROOT/bin/python -m ensurepip -U
$PY2_ROOT/bin/pip install -U setuptools pip wheel

echo "export PY2_ROOT=${PY2_ROOT}" > $IROOT/py2.installed
echo -e "export PYTHONHOME=\$PY2_ROOT" >> $IROOT/py2.installed
echo -e "export PATH=\$PY2_ROOT/bin:\$PATH" >> $IROOT/py2.installed

source $IROOT/py2.installed
