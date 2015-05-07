#!/bin/bash

sudo wget http://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
sudo apt-get update && sudo apt-get -y --allow-unauthenticated install --reinstall d-apt-keyring && sudo apt-get update
sudo apt-get install -qq -y dub
sudo apt-get install -qq -y libevent-dev libssl-dev
sudo apt-get install -qq -y g++ gcc-multilib xdg-utils
wget "http://downloads.dlang.org/releases/2014/dmd_2.066.1-0_amd64.deb"
sudo dpkg -i dmd_2.066.1-0_amd64.deb
cd $FWROOT/frameworks/D/vibed
dub build --build-mode=singleFile