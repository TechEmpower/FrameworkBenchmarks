#!/bin/bash

VALA_API_VERSION="0.38"
VALA_VERSION="0.38.8"

fw_depends flex bison libglib2.0-dev libgraphviz-dev

fw_installed vala && return 0

fw_get -O https://download.gnome.org/sources/vala/${VALA_API_VERSION}/vala-${VALA_VERSION}.tar.xz
fw_untar vala-${VALA_VERSION}.tar.xz
(
	cd vala-${VALA_VERSION}
	./configure --prefix=$IROOT/vala
	make
	make install
	mv $IROOT/vala/share/vala/vapi/* $IROOT/vala/share/vala-${VALA_API_VERSION}/vapi
	rmdir $IROOT/vala/share/vala/vapi
	ln -s $IROOT/vala/share/vala-${VALA_API_VERSION}/vapi $IROOT/vala/share/vala/vapi
)

echo -e "export LD_LIBRARY_PATH=${IROOT}/vala/lib:\$LD_LIBRARY_PATH" > $IROOT/vala.installed
echo -e "export PKG_CONFIG_PATH=${IROOT}/vala/lib/pkgconfig:\$PKG_CONFIG_PATH" >> $IROOT/vala.installed
echo -e "export PATH=${IROOT}/vala/bin:\$PATH" >> $IROOT/vala.installed

source $IROOT/vala.installed
