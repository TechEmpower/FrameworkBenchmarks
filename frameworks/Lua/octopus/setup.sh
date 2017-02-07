#!/bin/bash

fw_depends mysql lua

rm -rf octopus
git clone https://github.com/cyberz-eu/octopus.git
cd octopus
# January 4th, 2017
git checkout 0c4fc42198fed3a299c78d4b910188113d478bc5
cd ..

# The following 3 lines are a hacky way to get this framework working.
# Ideally, TFB-database would be enough in config.lua to get this working and the
# zlib fix needs to happen within the framework owner's repo
DB_HOST=$(grep -Pio '.+(?= TFB-database)' /etc/hosts)
sed -i s/TFB-database/${DB_HOST}/g config.lua
sed -i 's|zlib_version=1\.2\.10|zlib_version=1.2.11|g' octopus/bin/unix/server.sh

cp -avr app octopus/extensions
cp -vf config.lua octopus/extensions

cd octopus/bin/unix
. ./server.sh install
. ./server.sh build
. ./server.sh start
