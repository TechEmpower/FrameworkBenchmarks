#!/bin/bash

fw_installed clang-3.9 && return 0

sudo add-apt-repository -s "deb http://apt.llvm.org/`lsb_release -cs`/ llvm-toolchain-`lsb_release -cs`-3.9 main"
wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
sudo apt-get -yq update
sudo apt-get install -qqy clang-3.9 lldb-3.9

touch $IROOT/clang-3.9.installed

