#!/bin/bash

fw_depends nim nimble

RETCODE=$(fw_exists ${IROOT}/jester.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/jester.installed
  return 0; }

JESTER=$IROOT/jester

git clone https://github.com/dom96/jester.git
cd jester
# 2015-06-25
git checkout 71b8cc069a0d271d619c2dc41bc6479047885587
nimble update
# If /home/testrunner/.nimble/pkgs/jester exists, write over it.
echo 'y' | nimble install

echo "export JESTER_HOME=${JESTER}" > $IROOT/jester.installed

source $IROOT/jester.installed
