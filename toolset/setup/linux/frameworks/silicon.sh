#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/silicon.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/silicon.installed
  return 0; }
  
SILICON=$IROOT/silicon

git clone https://github.com/matt-42/silicon.git
cd silicon;
git checkout 383282990a0ba12f7a74cb9d2987c7666904b279
CXX=/usr/bin/clang++ ./install.sh $IROOT

echo "" > $IROOT/silicon.installed

source $IROOT/silicon.installed
