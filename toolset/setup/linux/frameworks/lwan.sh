#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/lwan.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/lwan.installed
  return 0; }

REV='49607addb31879e2aa2b701317773674662315aa'
LWAN_HOME=$IROOT/lwan

[ ! -e $IROOT/lwan.installed -a -d $LWAN_HOME ] && rm -rf $LWAN_HOME

# Lwan is only built during installation as a dependency sanity check.
git clone git://github.com/lpereira/lwan.git
cd $LWAN_HOME
git checkout ${REV}
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make techempower

echo "export LWAN_ROOT=${LWAN_HOME}" > $IROOT/lwan.installed
echo -e "export LWAN_BUILD=\$LWAN_ROOT/build" >> $IROOT/lwan.installed

source $IROOT/lwan.installed
