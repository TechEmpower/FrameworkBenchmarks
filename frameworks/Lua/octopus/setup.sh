#!/bin/bash


rm -rf octopus
git clone https://github.com/cyberz-eu/octopus.git
cp -avr app octopus/extensions
cp -vf config.lua octopus/extensions
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' octopus/extensions/config.lua

cd octopus/bin/unix
. ./server.sh install
. ./server.sh build
. ./server.sh start