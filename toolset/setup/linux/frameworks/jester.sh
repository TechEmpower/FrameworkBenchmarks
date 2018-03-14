#!/bin/bash

fw_depends nim nimble

fw_installed jester && return 0

JESTER=$IROOT/jester

git clone https://github.com/dom96/jester.git
cd jester
# 2018-02-28
git checkout 86744f7dff56522c9baa1b4f4381db44044abd9f
nimble update
# If ~/.nimble/pkgs/jester exists, write over it.
echo 'y' | nimble install

echo "export JESTER_HOME=${JESTER}" > $IROOT/jester.installed

source $IROOT/jester.installed
