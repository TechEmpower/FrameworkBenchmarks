#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists nimrod/bin/nimrod
[ $? -ne 0 ] || { echo "Nimrod is installed!"; return 0; }

test -d nimrod || git clone git://github.com/Araq/Nimrod.git nimrod
cd nimrod 
git checkout 987ac2439a87d74838233a7b188e4db340495ee5
test -d csources || git clone git://github.com/nimrod-code/csources.git

cd csources
git checkout 704015887981932c78a033dd5ede623b2ad6ae27
chmod +x build.sh
./build.sh

cd ../..
fw_exists nimrod/koch
[ $? -ne 0 ] || { echo "Nimrod Koch is installed!"; return 0; }

cd nimrod
bin/nimrod c koch
./koch boot -d:release
