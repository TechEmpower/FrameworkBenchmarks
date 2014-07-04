#!/bin/bash

# TODO check this path...
fw_exists /usr/bin/haskell-compiler
[ $? -ne 0 ] || { return 0; }

sudo apt-get install -y ghc cabal-install