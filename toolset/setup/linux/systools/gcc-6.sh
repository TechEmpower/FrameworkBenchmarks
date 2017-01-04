#!/bin/bash

fw_installed gcc-6 && return 0

sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
sudo apt-get -yq update
sudo apt-get install -qqy gcc-6

touch $IROOT/gcc-6.installed
