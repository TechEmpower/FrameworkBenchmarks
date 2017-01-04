#!/bin/bash

fw_installed yajl && return 0

VERSION="2.1.0"
ARCHIVE="${VERSION}.tar.gz"
BUILD_DIR="yajl-${VERSION}"
YAJL_HOME="${IROOT}/yajl"

pushd "${IROOT}"
fw_get -O "https://github.com/lloyd/yajl/archive/$ARCHIVE"
fw_untar "$ARCHIVE"
pushd "$BUILD_DIR"
./configure -p "$YAJL_HOME"
make -j "$(nproc)" install
popd
rm -rf "$BUILD_DIR"
popd

echo "export YAJL_HOME=$YAJL_HOME" > "${IROOT}/yajl.installed"
echo -e "export LD_LIBRARY_PATH=\$YAJL_HOME/lib:\$LD_LIBRARY_PATH" \
	>> "${IROOT}/yajl.installed"

source "${IROOT}/yajl.installed"
