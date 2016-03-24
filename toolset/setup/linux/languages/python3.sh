#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/py3.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/py3.installed
  return 0; }
  
PY3_ROOT=$IROOT/py3
PY3_VERSION=3.5.1

fw_get -O http://www.python.org/ftp/python/${PY3_VERSION}/Python-${PY3_VERSION}.tar.xz
fw_untar Python-${PY3_VERSION}.tar.xz
cd Python-${PY3_VERSION}
./configure --prefix=$PY3_ROOT --disable-shared --with-computed-gotos --quiet
make -j4 --quiet 2>&1 | tee $IROOT/python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
make install --quiet 2>&1 | tee -a $IROOT/python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
cd ..

$PY3_ROOT/bin/python3 -m ensurepip -U
$PY3_ROOT/bin/pip3 install -U setuptools pip wheel

echo "export PY3_ROOT=${PY3_ROOT}" > $IROOT/py3.installed
echo -e "export PYTHONHOME=\$PY3_ROOT" >> $IROOT/py3.installed
echo -e "export PATH=\$PY3_ROOT/bin:\$PATH" >> $IROOT/py3.installed

source $IROOT/py3.installed
