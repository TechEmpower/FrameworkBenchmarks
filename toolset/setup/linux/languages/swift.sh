#!/bin/bash

fw_installed swift && return 0

fw_depends clang-3.9

sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-3.9 100
sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.9 100

sudo apt-add-repository --yes ppa:george-edison55/cmake-3.x
sudo apt-get update -qq
sudo apt-get install -qqy cmake

# TODO: Use Swift 4.1 stable once it's released.
fw_get -O https://swift.org/builds/development/ubuntu1404/swift-DEVELOPMENT-SNAPSHOT-2018-01-30-a/swift-DEVELOPMENT-SNAPSHOT-2018-01-30-a-ubuntu14.04.tar.gz
fw_untar swift-DEVELOPMENT-SNAPSHOT-2018-01-30-a-ubuntu14.04.tar.gz
mv swift-DEVELOPMENT-SNAPSHOT-2018-01-30-a-ubuntu14.04 swift

echo -e "export PATH=${IROOT}/swift/usr/bin:\$PATH" >> $IROOT/swift.installed

source $IROOT/swift.installed
