#!/bin/bash

RETCODE=$(fw_exists nimrod/bin/nim)
[ ! "$RETCODE" == 0 ] || { return 0; }

test -d nimrod || git clone git://github.com/Araq/Nimrod.git nimrod
cd nimrod 
git checkout bbb16713c1bbc266cb8d69f262cc302641e361c4

test -d csources || git clone git://github.com/nimrod-code/csources.git
cd csources
git checkout b0bcf88e26730b23d22e2663adf1babb05bd5a71
sh build.sh
cd ..

bin/nimrod c koch

# compile nim
./koch boot -d:release

# nim's package manager
test -d nimble || git clone git://github.com/nimrod-code/nimble.git
cd nimble
git checkout ecd78e0e0300a8178db320d83014d3eb47a89b4c
../bin/nim c src/nimble
mv src/nimble ../bin/
