#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/silicon.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/silicon.installed
  return 0; }
  
SILICON=$IROOT/silicon

git clone https://github.com/matt-42/silicon.git
cd silicon;
git checkout 17167fd6065b1fd4e628a81f2327121d9f733298
CXX=/usr/bin/g++-4.9 ./install.sh $IROOT

echo "" > $IROOT/silicon.installed

source $IROOT/silicon.installed
