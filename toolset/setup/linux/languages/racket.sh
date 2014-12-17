#!/bin/bash

RET=$(fw_exists ${IROOT}/racket.installed)
if [ "$RET" == 0 ]; then 
  return 0;
fi

fw_get http://mirror.racket-lang.org/installers/recent/racket-src.tgz -O racket-src.tar.gz
fw_untar racket-src.tar.gz
mv racket racket-install
cd racket-install/src 
./configure --prefix=${IROOT}/racket
make
make install

touch ${IROOT}/racket.installed