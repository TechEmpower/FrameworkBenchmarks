#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/octane.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/octane.installed
  return 0; }

OCTANE_HOME=$IROOT/octane

git clone https://github.com/simongui/octane.git
cd $OCTANE_HOME
# December 21st, 2016
git checkout 8a5f4b11d3c84ed02c52f2592f4a7b72d6a8831e
make

echo "export OCTANE_HOME=${OCTANE_HOME}" >> $IROOT/octane.installed
echo -e "export PATH=\$OCTANE_HOME/build/:\$PATH" >> $IROOT/octane.installed

source $IROOT/octane.installed
