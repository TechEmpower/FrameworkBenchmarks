#!/bin/bash

RETCODE=$(fw_exists $IROOT/nim.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

test -d nim || git clone git://github.com/Araq/Nim.git nim
cd nim
# post version 0.10.2 - most recent as of 2014-12-
git checkout v0.10.2

test -d csources || git clone git://github.com/nim-lang/csources.git
cd csources
sh build.sh
cd ..

bin/nim c koch

# bootstrapping nim's compiler
./koch boot -d:release

# nim's package manager
test -d nimble || git clone git://github.com/nim-lang/nimble.git
cd nimble
git checkout v0.6
../bin/nim c src/nimble
mv src/nimble ../bin/

touch $IROOT/nim.installed
