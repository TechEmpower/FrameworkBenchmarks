#!/bin/bash

RETCODE=$(fw_exists nimrod/bin/nimrod)
[ ! "$RETCODE" == 0 ] || { return 0; }

test -d nimrod || git clone git://github.com/Araq/Nimrod.git nimrod
cd nimrod 
git checkout 887a1ebe688a01259263ad6e11c9061cfc940456
test -d csources || git clone git://github.com/nimrod-code/csources.git

cd csources
git checkout 0a6e5758ed16bf906fcfdb9e64086edd9b60c5c0
chmod +x build.sh
./build.sh

cd ../..
RETCODE=$(fw_exists nimrod/koch)
[ ! "$RETCODE" == 0 ] || { return 0; }

cd nimrod
bin/nimrod c koch
./koch boot -d:release
sudo ./koch install /usr/bin
