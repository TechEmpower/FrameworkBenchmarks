#!/bin/bash -ex

RETCODE=$(fw_exists /opt/ghc/7.8.3/bin/ghc)
[ ! "$RETCODE" == 0 ] || { return 0; }

lsb_release -a

sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y ghc-7.8.3 cabal-install-1.20 libpcre3-dev
