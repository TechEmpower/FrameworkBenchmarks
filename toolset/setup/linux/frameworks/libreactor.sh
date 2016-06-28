#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/libreactor.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/libreactor.installed
  return 0; }

LIBREACTOR_HOME=$IROOT/libreactor_techempower

git clone https://github.com/fredrikwidlund/libreactor_techempower
cd $LIBREACTOR_HOME
export CC=gcc-4.9
export RANLIB=gcc-ranlib-4.9
export AR=gcc-ar-4.9
./autogen.sh
./configure
make

echo "export LIBREACTOR_HOME=${LIBREACTOR_HOME}" >> $IROOT/libreactor.installed
echo -e "export PATH=\$LIBREACTOR_HOME/:\$PATH" >> $IROOT/libreactor.installed

source $IROOT/libreactor.installed
