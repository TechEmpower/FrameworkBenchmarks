#!/bin/bash

# install swift
wget https://swift.org/builds/development/ubuntu1404/swift-DEVELOPMENT-SNAPSHOT-2016-03-24-a/swift-DEVELOPMENT-SNAPSHOT-2016-03-24-a-ubuntu14.04.tar.gz
tar -zxvf swift-DEVELOPMENT-SNAPSHOT-2016-03-24-a-ubuntu14.04.tar.gz 
mv swift-DEVELOPMENT-SNAPSHOT-2016-03-24-a-ubuntu14.04 swift
export PATH=$PWD/swift/usr/bin/:"{$PATH}"

# install Swift compiler dependencies
sudo apt-get -y install clang libicu-dev

# compile the project with optimizations
swift build -c release

# run in the background
.build/release/App &

