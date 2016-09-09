#!/bin/bash

fw_depends clang-3.8 gcc-4.9

RETCODE=$(fw_exists ${IROOT}/silicon.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/silicon.installed
  return 0; }

SILICON=$IROOT/silicon

git clone https://github.com/matt-42/silicon.git
cd silicon;
# May 18th, 2016
git checkout 73dac7f3c8dcd4f9c53713456e8b73165006e968
CC=clang-3.8 CXX=clang++-3.8 ./install.sh $IROOT

echo "" > $IROOT/silicon.installed

source $IROOT/silicon.installed
