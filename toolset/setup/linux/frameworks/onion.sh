#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/onion.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/onion.installed
  return 0; }

git clone https://github.com/davidmoreno/onion.git
cd $IROOT/onion

# Latest commit on master as of July 10 2014
# This is post tag v0.7, but pre any later tags
git checkout c460557bfc7d45fb6ba61cb6b7259480a67dde82

mkdir -p build
cd build
cmake ..
make

echo "export ONION_LOG=noinfo" > $IROOT/onion.installed

source $IROOT/onion.installed
