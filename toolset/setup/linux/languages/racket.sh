#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists racket-5.3.6/bin/racket
ret1=$?
fw_exists racket-5.3.6/collects/racket
ret2=$?
if [ $ret1 -eq 0 ] && [$ret2 -eq 0 ]; then 
  echo "Racket is installed!"; 
  return 0;
fi

fw_get https://github.com/plt/racket/archive/v5.3.6.tar.gz -O racket-5.3.6.tar.gz
fw_untar racket-5.3.6.tar.gz
cd racket-5.3.6/src 
./configure
make
sudo make install
