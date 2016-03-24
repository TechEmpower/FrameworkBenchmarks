#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/silicon.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/silicon.installed
  return 0; }
  
SILICON=$IROOT/silicon

git clone https://github.com/matt-42/silicon.git
cd silicon;
git checkout a2b930696a72aa963056f21b1605adfe8ec1a8a7
CXX=clang++-3.5 ./install.sh $IROOT

echo "" > $IROOT/silicon.installed

source $IROOT/silicon.installed
