#!/bin/bash

RETCODE=$(fw_exists /usr/bin/erl)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo cp $FWROOT/config/erlang.list /etc/apt/sources.list.d/erlang.list

fw_get http://binaries.erlang-solutions.com/debian/erlang_solutions.asc

sudo apt-key add erlang_solutions.asc
sudo apt-get -y update
sudo apt-get install -y esl-erlang