#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/haywire.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/haywire.installed
  return 0; }

HAYWIRE_HOME=$IROOT/Haywire
 
git clone https://github.com/kellabyte/Haywire.git
cd $HAYWIRE_HOME
./build.sh -c release

echo "export HAYWIRE_HOME=${HAYWIRE_HOME}" >> $IROOT/haywire.installed
echo -e "export PATH=\$HAYWIRE_HOME/builds/unix/release/:\$PATH" >> $IROOT/haywire.installed

source $IROOT/haywire.installed
