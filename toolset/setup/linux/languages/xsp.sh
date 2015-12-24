#!/bin/bash

fw_depends mono

echo "!!!!!!!!!! XSP START !!!!!!!!!!"

RETCODE=$(fw_exists ${IROOT}/xsp.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/xsp.installed
  return 0; }

echo "!!!!!!!!!! XSP get !!!!!!!!!!"
# get
git clone git://github.com/mono/xsp
cd xsp
git checkout e272a2c006211b6b03be2ef5bbb9e3f8fefd0768

echo "!!!!!!!!!! XSP build !!!!!!!!!!"
# build
./autogen.sh --prefix=$MONO_HOME --disable-docs
echo "!!!!!!!!!! XSP build POINT1 !!!!!!!!!!"
make
echo "!!!!!!!!!! XSP build POINT2 !!!!!!!!!!"
sudo make install

echo "!!!!!!!!!! XSP cleanup !!!!!!!!!!"
# cleanup
cd ..
rm -rf xsp

echo "" > $IROOT/xsp.installed

echo "!!!!!!!!!! XSP END !!!!!!!!!!"
