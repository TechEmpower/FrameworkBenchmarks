#!/bin/bash

REV='49607addb31879e2aa2b701317773674662315aa'

LWAN_HOME=$IROOT/lwan
RETCODE=$(fw_exists ${LWAN_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $LWAN_HOME.installed
  return 0; }

[ ! -e $LWAN_HOME.installed -a -d $LWAN_HOME ] && rm -rf $LWAN_HOME

# Lwan is only built during installation as a dependency sanity check.
git clone git://github.com/lpereira/lwan.git
cd $LWAN_HOME
git checkout ${REV}
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make techempower

echo "export LWAN_ROOT=${LWAN_HOME}" > $LWAN_HOME.installed
echo "export LWAN_BUILD=${LWAN_HOME}/build" >> $LWAN_HOME.installed

source $LWAN_HOME.installed
