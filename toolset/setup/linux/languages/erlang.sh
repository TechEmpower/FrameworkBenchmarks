#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/erlang.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb

sudo apt-get -y update
sudo apt-get install -y esl-erlang

touch ${IROOT}/erlang.installed