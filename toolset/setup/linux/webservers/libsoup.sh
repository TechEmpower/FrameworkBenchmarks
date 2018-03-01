#!/bin/bash

fw_depends glib-networking libsqlite3-dev intltool

fw_installed libsoup && return 0

fw_get -O https://download.gnome.org/sources/libsoup/2.60/libsoup-2.60.3.tar.xz
fw_untar libsoup-2.60.3.tar.xz
cd libsoup-2.60.3
./configure --prefix=$IROOT/libsoup
make
make install

echo -e "export LD_LIBRARY_PATH=${IROOT}/libsoup/lib:\$LD_LIBRARY_PATH" > $IROOT/libsoup.installed
echo -e "export PKG_CONFIG_PATH=${IROOT}/libsoup/lib/pkgconfig:\$PKG_CONFIG_PATH" >> $IROOT/libsoup.installed

source $IROOT/libsoup.installed
