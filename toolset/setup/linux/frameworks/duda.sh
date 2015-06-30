#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/duda.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/duda.installed
  return 0; }

git clone https://github.com/monkey/dudac.git

DUDA_HOME=$IROOT/dudac
cd $DUDA_HOME

# Get v0.31 (no official releases that work 2015-06-25)
git checkout 7c3d5b03b09fb4cb5f5e338fff72df2e25e95ef0

./dudac -r
./dudac -s

echo "export DUDA_HOME=${DUDA_HOME}" > $IROOT/duda.installed
echo -e "export PATH=\$DUDA_HOME:$PATH" >> $IROOT/duda.installed

source $IROOT/duda.installed
