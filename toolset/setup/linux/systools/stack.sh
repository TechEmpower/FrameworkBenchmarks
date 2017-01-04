#!/bin/bash

fw_installed stack && return 0

fw_get -o $IROOT/stack.tar.gz https://www.stackage.org/stack/linux-x86_64
tar xzf $IROOT/stack.tar.gz
pushd $IROOT/stack-*
mv stack $IROOT
popd
sudo apt-get -y install perl make automake gcc libgmp3-dev

touch $IROOT/stack.installed

source $IROOT/stack.installed
