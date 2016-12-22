#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/octane.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/octane.installed
  return 0; }

OCTANE_HOME=$IROOT/octane

git clone https://github.com/simongui/octane.git
cd $OCTANE_HOME
make

echo "export OCTANE_HOME=${OCTANE_HOME}" >> $IROOT/octane.installed
echo -e "export PATH=\$OCTANE_HOME/build/:\$PATH" >> $IROOT/octane.installed

source $IROOT/octane.installed
