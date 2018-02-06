#!/bin/bash

fw_depends libboost-dev clang-3.9 gcc-6

fw_installed silicon && return 0

SILICON=$IROOT/silicon

git clone https://github.com/matt-42/silicon.git
cd silicon;
git checkout ecaf04887c9dbbf0f457afab1f487268f6aeffab
CC=clang-3.9 CXX=clang++-3.9 ./install.sh $IROOT

echo "" > $IROOT/silicon.installed

source $IROOT/silicon.installed
