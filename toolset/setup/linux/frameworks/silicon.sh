#!/bin/bash

fw_depends libboost-dev clang-3.9 gcc-4.9

fw_installed silicon && return 0

SILICON=$IROOT/silicon

git clone https://github.com/matt-42/silicon.git
cd silicon;
# December 28th, 2016
git checkout 1fed7cead9490e3054af730ac9bf04ae4cf5009d
CC=clang-3.9 CXX=clang++-3.9 ./install.sh $IROOT

echo "" > $IROOT/silicon.installed

source $IROOT/silicon.installed
