#!/bin/bash

RETCODE=$(fw_exists $IROOT/rebar.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends erlang
export PATH=$PATH:$IROOT/erlang/bin

fw_get https://github.com/rebar/rebar/archive/2.5.1.tar.gz -O rebar-2.5.1.tar.gz
fw_untar rebar-2.5.1.tar.gz
mv rebar-2.5.1 rebar
cd rebar
./bootstrap

touch $IROOT/rebar.installed
