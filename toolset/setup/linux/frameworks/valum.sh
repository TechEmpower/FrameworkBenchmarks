#!/bin/bash

VALUM_VERSION="0.3.12"

fw_depends meson ninja vala

fw_installed valum && return 0

sudo apt-get install -y libglib2.0-dev libsoup2.4-dev

fw_get -O https://github.com/valum-framework/valum/archive/v${VALUM_VERSION}.tar.gz
fw_untar v${VALUM_VERSION}.tar.gz
(
	cd valum-${VALUM_VERSION}
	rm -rf build
	meson --prefix=${IROOT}/vala --buildtype=release --libdir=lib build # install along Vala for convenience
	ninja -C build
	ninja -C build install
)

touch ${IROOT}/valum.installed
