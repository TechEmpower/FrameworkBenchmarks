#!/bin/bash

fw_exists /usr/bin/treefrog
ret1=$?
fw_exists /usr/bin/tspawn
ret2=$?
if [ $ret1 -eq 0 ] && [ $ret2 -eq 0 ]; then 
  return 0;
fi

sudo apt-get install -y qt4-qmake libqt4-dev libqt4-sql-mysql libqt4-sql-psql g++

fw_get http://downloads.sourceforge.net/project/treefrog/src/treefrog-1.7.5.tar.gz
fw_untar treefrog-1.7.5.tar.gz
cd treefrog-1.7.5
./configure

cd src
make
sudo make install

cd ../tools
make
sudo make install
