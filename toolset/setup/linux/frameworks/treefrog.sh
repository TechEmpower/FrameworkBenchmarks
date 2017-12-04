#!/bin/bash

fw_installed treefrog && return 0

TFVER=1.19.0

sudo apt-get update -qq
sudo apt-get install -y g++ gcc libjemalloc-dev
sudo apt-get install -y qt5-qmake qt5-default qtbase5-dev qtbase5-dev-tools libqt5sql5 libqt5sql5-mysql libqt5sql5-psql libqt5qml5 libqt5xml5 qtdeclarative5-dev libqt5quick5 libqt5quickparticles5 libqt5gui5 libqt5printsupport5 libqt5widgets5 libqt5opengl5-dev libqt5quicktest5
fw_get -O https://github.com/treefrogframework/treefrog-framework/archive/v${TFVER}.tar.gz
fw_untar v${TFVER}.tar.gz
cd treefrog-framework-${TFVER}
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
