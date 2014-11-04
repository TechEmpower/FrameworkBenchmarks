#!/bin/bash

RETCODE=$(fw_exists $IROOT/nim.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

test -d nimrod || git clone git://github.com/Araq/Nimrod.git nimrod
cd nimrod 
# post version 0.9.6 - most recent as of 2014-10-31
git checkout bbb16713c1bbc266cb8d69f262cc302641e361c4

test -d csources || git clone git://github.com/nimrod-code/csources.git
cd csources
# post version 0.9.6 - most recent as of 2014-10-31
git checkout b0bcf88e26730b23d22e2663adf1babb05bd5a71
sh build.sh
cd ..

bin/nimrod c koch

# bootstrapping nim's compiler
./koch boot -d:release

# nim's package manager
test -d nimble || git clone git://github.com/nimrod-code/nimble.git
cd nimble
# post version 0.4.0 - most recent as of 2014-10-31
git checkout ecd78e0e0300a8178db320d83014d3eb47a89b4c
../bin/nim c src/nimble
mv src/nimble ../bin/

touch $IROOT/nim.installed
