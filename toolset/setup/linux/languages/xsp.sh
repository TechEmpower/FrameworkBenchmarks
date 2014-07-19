#!/bin/bash

RETCODE=$(fw_exists xsp)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends mono
git clone --depth 1 git://github.com/mono/xsp
cd xsp
git checkout 8a31bc625727594d42f94173768bee5cf8afd0a4
./autogen.sh --prefix=/usr/local
make
sudo make install
