#!/bin/bash

RET1=$(fw_exists racket-5.3.6/bin/racket)
RET2=$(fw_exists racket-5.3.6/collects/racket)
if [ "$RET1" == 0 ] && [ "$RET2" == 0 ]; then 
  return 0;
fi

fw_get http://mirror.racket-lang.org/installers/recent/racket-src.tgz -O racket-src.tar.gz
fw_untar racket-src.tar.gz
cd racket/src 
./configure --prefix=/usr/local
make
sudo make install
