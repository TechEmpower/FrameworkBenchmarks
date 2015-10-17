#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/treefrog.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/treefrog.installed
  return 0; }

sudo apt-get install -y qt4-qmake libqt4-dev libqt4-sql-mysql libqt4-sql-psql g++

fw_get -O http://downloads.sourceforge.net/project/treefrog/src/treefrog-1.7.7.tar.gz
fw_untar treefrog-1.7.7.tar.gz
cd treefrog-1.7.7
# TODO: Someday we can try this... I couldn't get it working
#./configure --prefix=$IROOT/treefrog
./configure

cd src
make -j4
sudo make install

cd ../tools
make -j4
sudo make install

echo "" > $IROOT/treefrog.installed

source $IROOT/treefrog.installed
