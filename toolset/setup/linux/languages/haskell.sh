#!/bin/bash 

export LANG=en_US.UTF-8

sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y ghc-7.8.3 cabal-install-1.20 libpcre3-dev