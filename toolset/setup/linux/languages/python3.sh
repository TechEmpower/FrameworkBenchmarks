#!/bin/bash

PY3_ROOT=$IROOT/py3
RETCODE=$(fw_exists ${PY3_ROOT}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $PY3_ROOT.installed
  return 0; }

fw_get http://www.python.org/ftp/python/3.4.2/Python-3.4.2.tar.xz
fw_untar Python-3.4.2.tar.xz
cd Python-3.4.2
./configure --prefix=$PY3_ROOT --disable-shared --with-computed-gotos --quiet
make -j4 --quiet 2>&1 | tee $IROOT/python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
make install --quiet 2>&1 | tee -a $IROOT/python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
cd ..

$IROOT/py3/bin/python3 -m ensurepip -U
$IROOT/py3/bin/pip3 install -U setuptools pip

echo "export PY3_ROOT=${PY3_ROOT}" > $PY3_ROOT.installed
echo "export PYTHONHOME=${PY3_ROOT}" >> $PY3_ROOT.installed
echo -e "export PATH=${PY3_ROOT}/bin:\$PATH" >> $PY3_ROOT.installed

source $PY3_ROOT.installed
