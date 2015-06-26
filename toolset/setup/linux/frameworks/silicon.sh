#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/silicon.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/silicon.installed
  return 0; }
  
SILICON=$IROOT/silicon

git clone https://github.com/matt-42/silicon.git
cd silicon;
git checkout df56f30f8db8d3be8305f56e198818e83a14c985
CXX=/usr/bin/g++-4.9 ./install.sh $IROOT

echo "" > $IROOT/silicon.installed

source $IROOT/silicon.installed
