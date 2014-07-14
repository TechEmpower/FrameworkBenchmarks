#!/bin/bash

RETCODE=$(fw_exists treefrog-1.7.7)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y qt4-qmake libqt4-dev libqt4-sql-mysql libqt4-sql-psql g++

fw_get http://downloads.sourceforge.net/project/treefrog/src/treefrog-1.7.7.tar.gz
fw_untar treefrog-1.7.7.tar.gz
rm -f treefrog-1.7.7.tar.gz
cd treefrog-1.7.7
./configure

cd src
make -j4
sudo make install

cd ../tools
make -j4
sudo make install
