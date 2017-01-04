#!/bin/bash

fw_installed octane && return 0

OCTANE_HOME=$IROOT/octane

git clone https://github.com/simongui/octane.git
cd $OCTANE_HOME
# December 21st, 2016
git checkout 04c95e3ff650b9d8f06e84f9c135b7c6a9ad4eb6
make

echo "export OCTANE_HOME=${OCTANE_HOME}" >> $IROOT/octane.installed
echo -e "export PATH=\$OCTANE_HOME/build/:\$PATH" >> $IROOT/octane.installed

source $IROOT/octane.installed
