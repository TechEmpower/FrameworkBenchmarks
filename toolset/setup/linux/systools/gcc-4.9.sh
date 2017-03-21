#!/bin/bash

fw_installed gcc-4.9 && return 0

sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
sudo apt-get -yq update
sudo apt-get install -qqy gcc-4.9 g++-4.9

touch $IROOT/gcc-4.9.installed
