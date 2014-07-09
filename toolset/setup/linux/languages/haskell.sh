#!/bin/bash

RETCODE=$(fw_exists /usr/bin/haskell-compiler)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y ghc cabal-install