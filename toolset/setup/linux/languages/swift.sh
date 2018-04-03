#!/bin/bash

fw_installed swift && return 0

fw_depends clang-3.9

sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-3.9 100
sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.9 100

sudo apt-add-repository --yes ppa:george-edison55/cmake-3.x
sudo apt-get update -qq
sudo apt-get install -qqy cmake

fw_get -O https://swift.org/builds/swift-4.1-release/ubuntu1404/swift-4.1-RELEASE/swift-4.1-RELEASE-ubuntu14.04.tar.gz
fw_untar swift-4.1-RELEASE-ubuntu14.04.tar.gz
mv swift-4.1-RELEASE-ubuntu14.04 swift

echo -e "export PATH=${IROOT}/swift/usr/bin:\$PATH" >> $IROOT/swift.installed

source $IROOT/swift.installed
