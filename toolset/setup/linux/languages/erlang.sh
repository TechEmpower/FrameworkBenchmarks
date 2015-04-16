#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/erlang.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/erlang.installed
  return 0; }

fw_get http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb

sudo apt-get -y update
sudo apt-get install -y esl-erlang

echo "" > $IROOT/erlang.installed

source $IROOT/erlang.installed
