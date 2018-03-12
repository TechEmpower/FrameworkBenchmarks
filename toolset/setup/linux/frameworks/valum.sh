#!/bin/bash

VALUM_VERSION="0.3.15"

fw_depends meson ninja vala libsoup

fw_installed valum && return 0

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
