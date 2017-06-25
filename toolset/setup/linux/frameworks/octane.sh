#!/bin/bash

fw_installed octane && return 0

OCTANE_HOME=$IROOT/octane

git clone https://github.com/simongui/octane.git
cd $OCTANE_HOME
# May 22nd, 2017
git checkout 8c28b1b83f1aa2817a401a3e8437a0af4ec53c28
make

echo "export OCTANE_HOME=${OCTANE_HOME}" >> $IROOT/octane.installed
echo -e "export PATH=\$OCTANE_HOME/build/:\$PATH" >> $IROOT/octane.installed

source $IROOT/octane.installed
