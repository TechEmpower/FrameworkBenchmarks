#!/bin/bash 

RETCODE=$(fw_exists ${IROOT}/haskell.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/haskell.installed
  return 0; }

CABAL_HOME=/opt/cabal/1.20
HASKELL_HOME=/opt/ghc/7.8.3

# TODO: someday move away from apt
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y ghc-7.8.3 cabal-install-1.20 libpcre3-dev

echo "export LANG=en_US.UTF-8" > $IROOT/haskell.installed
echo "export CABAL_HOME=${CABAL_HOME}" >> $IROOT/haskell.installed
echo "export HASKELL_HOME=${HASKELL_HOME}" >> $IROOT/haskell.installed
echo -e "export PATH=\$HASKELL_HOME/bin:\$CABAL_HOME/bin:\$PATH" >> $IROOT/haskell.installed

source $IROOT/haskell.installed
