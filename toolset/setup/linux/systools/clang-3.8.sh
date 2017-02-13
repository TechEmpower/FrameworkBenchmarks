#!/bin/bash


fw_installed clang-3.8 && return 0

sudo add-apt-repository -s "deb http://apt.llvm.org/`lsb_release -cs`/ llvm-toolchain-`lsb_release -cs`-3.8 main"
wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
sudo apt-get -yq update
sudo apt-get install -qqy clang-3.8 lldb-3.8

touch $IROOT/clang-3.8.installed

