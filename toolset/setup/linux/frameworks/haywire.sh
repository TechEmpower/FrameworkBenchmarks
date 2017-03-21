#!/bin/bash

fw_installed haywire && return 0

HAYWIRE_HOME=$IROOT/Haywire
 
git clone https://github.com/kellabyte/Haywire.git
cd $HAYWIRE_HOME
git checkout e4d23b533c15ddc51966e439fa455eb764e0f555
./build.sh -c release

echo "export HAYWIRE_HOME=${HAYWIRE_HOME}" >> $IROOT/haywire.installed
echo -e "export PATH=\$HAYWIRE_HOME/builds/unix/release/:\$PATH" >> $IROOT/haywire.installed

source $IROOT/haywire.installed
