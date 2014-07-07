#!/bin/bash

RET1=$(fw_exists racket-5.3.6/bin/racket)
RET2=$(fw_exists racket-5.3.6/collects/racket)
if [ "$RET1" == 0 ] && [ "$RET2" == 0 ]; then 
  return 0;
fi

fw_get https://github.com/plt/racket/archive/v5.3.6.tar.gz -O racket-5.3.6.tar.gz
fw_untar racket-5.3.6.tar.gz
cd racket-5.3.6/src 
./configure
make
sudo make install
