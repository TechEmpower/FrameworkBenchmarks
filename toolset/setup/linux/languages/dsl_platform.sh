#!/bin/bash

RETCODE=$(fw_exists $IROOT/dsl-compiler.exe)
[ ! "$RETCODE" == 0 ] || { \
  return 0; }

wget -O $IROOT/dsl-compiler.zip https://github.com/ngs-doo/revenj/releases/download/1.3.1/dsl-compiler.zip
unzip $IROOT/dsl-compiler.zip -d $IROOT
rm $IROOT/dsl-compiler.zip
