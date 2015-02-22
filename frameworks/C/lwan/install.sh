#!/bin/bash

REV='8ae98d2b6e146948a7ed13e76141e0f69228d690'

INSTALLED_FILE="${IROOT}/lwan-${REV}.installed"
RETCODE=$(fw_exists ${INSTALLED_FILE})
[ ! "$RETCODE" == 0 ] || { return 0; }

[ ! -e ${INSTALLED_FILE} -a -d ${IROOT}/lwan ] && rm -rf ${IROOT}/lwan

# Lwan is only built during installation as a dependency sanity check.
git clone git://github.com/lpereira/lwan.git
cd lwan
git checkout ${REV}
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make techempower

touch ${INSTALLED_FILE}
