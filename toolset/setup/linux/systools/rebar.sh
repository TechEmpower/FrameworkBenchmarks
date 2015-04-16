#!/bin/bash

REBAR_HOME=$IROOT/rebar
RETCODE=$(fw_exists ${REBAR_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $REBAR_HOME.installed
  return 0; }

fw_get https://github.com/rebar/rebar/archive/2.5.1.tar.gz -O rebar-2.5.1.tar.gz
fw_untar rebar-2.5.1.tar.gz
mv rebar-2.5.1 rebar
cd rebar
./bootstrap

echo "export REBAR_HOME=${REBAR_HOME}" > $REBAR_HOME.installed
echo -e "export PATH=${REBAR_HOME}:\$PATH" >> $REBAR_HOME.installed

source $REBAR_HOME.installed
