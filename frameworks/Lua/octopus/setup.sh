#!/bin/bash

fw_depends mysql

rm -rf octopus
git clone https://github.com/cyberz-eu/octopus.git
cd octopus
# November 1st, 2016
git checkout d123ba46d352fafebd74cb8ebeddc4810ba2c68b
cd ..

# Patch for fixing zlib version
sed -i 's|zlib_version=1\.2\.8|zlib_version=1.2.10|g' octopus/bin/unix/server.sh

cp -avr app octopus/extensions
cp -vf config.lua octopus/extensions
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' octopus/extensions/config.lua

cd octopus/bin/unix
. ./server.sh install
. ./server.sh build
. ./server.sh start
