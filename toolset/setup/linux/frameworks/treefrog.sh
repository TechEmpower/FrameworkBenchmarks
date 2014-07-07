#!/bin/bash

RET1=$(fw_exists /usr/bin/treefrog)
RET2=$(fw_exists /usr/bin/tspawn)
if [ "$RET1" == 0 ] && [ "$RET2" == 0 ]; then 
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
