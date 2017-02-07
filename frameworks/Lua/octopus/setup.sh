#!/bin/bash

fw_depends mysql lua

rm -rf octopus
git clone https://github.com/cyberz-eu/octopus.git
cd octopus
# January 4th, 2017
git checkout 0c4fc42198fed3a299c78d4b910188113d478bc5
cd ..

# The following line is a hacky way to get this framework working.
# zlib fix needs to happen within the framework owner's repo

sed -i 's|zlib_version=1\.2\.10|zlib_version=1.2.11|g' octopus/bin/unix/server.sh

cp -avr app octopus/extensions
cp -vf config.lua octopus/extensions

sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' octopus/extensions/config.lua

cd octopus/bin/unix
. ./server.sh install
. ./server.sh build
. ./server.sh start
