#!/bin/bash

fw_depends mysql lua

rm -rf octopus
git clone https://github.com/cyberz-eu/octopus.git
cd octopus
# January 4th, 2017
git checkout 0c4fc42198fed3a299c78d4b910188113d478bc5
cd ..

cp -avr app octopus/extensions
cp -vf config.lua octopus/extensions

cd octopus/bin/unix
. ./server.sh install
. ./server.sh build
. ./server.sh start
