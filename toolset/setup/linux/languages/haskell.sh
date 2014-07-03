#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

# TODO check this path...
fw_exists /usr/bin/haskell-compiler
[ $? -ne 0 ] || { echo "Haskell is installed!"; return 0; }

sudo apt-get install -y ghc cabal-install