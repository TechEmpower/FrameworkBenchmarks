#!/bin/bash

TFVER=treefrog-1.10.0
RETCODE=$(fw_exists ${IROOT}/treefrog.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/treefrog.installed
  return 0; }

sudo add-apt-repository --yes ppa:ubuntu-sdk-team/ppa
sudo apt-get update
sudo apt-get install -y qt5-qmake qt5-default qtbase5-dev qtbase5-dev-tools libqt5sql5 libqt5sql5-mysql libqt5sql5-psql g++ 
sudo add-apt-repository --remove --yes ppa:ubuntu-sdk-team/ppa

fw_get -O http://downloads.sourceforge.net/project/treefrog/src/$TFVER.tar.gz
fw_untar $TFVER.tar.gz
cd $TFVER
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
