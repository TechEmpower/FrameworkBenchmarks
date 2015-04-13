#!/bin/bash

HAYWIRE_HOME=$IROOT/Haywire
RETCODE=$(fw_exists ${HAYWIRE_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $HAYWIRE_HOME.installed
  return 0; }
 
git clone https://github.com/kellabyte/Haywire.git
cd $HAYWIRE_HOME
./build.sh -c release

echo "export PATH=${HAYWIRE_HOME}/builds/unix/release/:$PATH" > $HAYWIRE_HOME.installed

source $HAYWIRE_HOME.installed
