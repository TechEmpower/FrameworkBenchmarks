#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/cppsp.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y postgresql-server-dev-9.3 libpq-dev

fw_get -O cppsp_0.2.3.tar.xz http://downloads.sourceforge.net/project/cpollcppsp/CPPSP%200.2%20%28testing%29/cppsp_0.2.3.tar.xz
fw_untar cppsp_0.2.3.tar.xz

# Using cp+rm over mv intentionally, because apparently this download
# causes oddball issues when mv'ed around inside a folder mounted 
# inside of VirtualBox (may have something to do with case-sensitive 
# filesystems)
cp -R cppsp_rel0.2.3/ ${IROOT}/cppsp_0.2.3
rm -rf cppsp_rel0.2.3/

sed -i 's|CXX := .*|CXX := g++-4.8|g' ${IROOT}/cppsp_0.2.3/makefile
sed -i 's|-Wall|-w|g' ${IROOT}/cppsp_0.2.3/makefile

touch ${IROOT}/cppsp.installed