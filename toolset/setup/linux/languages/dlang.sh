#!/bin/bash

DLANG=$IROOT/dlang
RETCODE=$(fw_exists ${DLANG}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $DLANG.installed
  return 0; }

mkdir -p $DLANG
fw_get -O http://downloads.dlang.org/releases/2.x/2.067.1/dmd_2.067.1-0_amd64.deb
dpkg-deb -x dmd_2.067.1-0_amd64.deb $DLANG

# According to this file (dmd.conf) dmd will, upon execution, look for
# a dmd.conf in 1) the current working directory [bad], 2) the directory
# specified by the HOME environment variable [bad], 3) the directory in
# which dmd resides [less bad], and 4) the /etc directory.
# We are trying to maintain as little global presence as possible, so
# we need to change the DFLAGS in the dmd.conf to be correctly sandboxed
# to the $DLANG folder (in IROOT).
cp $DLANG/etc/dmd.conf $DLANG/usr/bin
sed -i "s|-I/usr/|-I${DLANG}/usr/|g" $DLANG/usr/bin/dmd.conf
sed -i "s|-L/usr/|-L${DLANG}/usr/|g" $DLANG/usr/bin/dmd.conf

echo -e "export PATH=${DLANG}/usr/bin:\$PATH" > $DLANG.installed

source $DLANG.installed
