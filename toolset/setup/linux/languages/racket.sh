#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/racket.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/racket.installed
  return 0; }

RACKET=$IROOT/racket

fw_get -o racket-src.tar.gz http://mirror.racket-lang.org/installers/recent/racket-src.tgz
fw_untar racket-src.tar.gz
mv racket racket-install
cd racket-install/src 
./configure --prefix=$RACKET
make
make install

echo "export RACKET_HOME=${RACKET}" > $IROOT/racket.installed
echo -e "export PATH=\$RACKET_HOME/bin:\$PATH" >> $IROOT/racket.installed

source $IROOT/racket.installed
