#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/bin/erl
[ $? -ne 0 ] || { echo "Erlang is installed!"; return 0; }

sudo cp ../config/erlang.list /etc/apt/sources.list.d/erlang.list

fw_get http://binaries.erlang-solutions.com/debian/erlang_solutions.asc
fw_untar node-v0.10.8-linux-x64.tar.gz

sudo apt-key add erlang_solutions.asc
sudo apt-get -y update
sudo apt-get install -y esl-erlang