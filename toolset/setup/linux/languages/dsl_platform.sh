#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/dsl-compiler-1.8.installed)
[ ! "$RETCODE" == 0 ] || { \
  return 0; }

wget -O $IROOT/dsl-compiler.zip https://github.com/ngs-doo/revenj/releases/download/1.4.2/dsl-compiler.zip
unzip -o $IROOT/dsl-compiler.zip -d $IROOT
rm $IROOT/dsl-compiler.zip

echo "1.8" > $IROOT/dsl-compiler-1.8.installed
