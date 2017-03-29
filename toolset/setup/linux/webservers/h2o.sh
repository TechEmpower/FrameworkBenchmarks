#!/bin/bash

fw_installed h2o && return 0

H2O_HOME="${IROOT}/h2o"
VERSION="2.1.0"
ARCHIVE="v${VERSION}.tar.gz"
BUILD_DIR="h2o-${VERSION}"

pushd "${IROOT}"
fw_get -O "https://github.com/h2o/h2o/archive/$ARCHIVE"
fw_untar "$ARCHIVE"
pushd "$BUILD_DIR"
cmake -DCMAKE_INSTALL_PREFIX="$H2O_HOME" -DCMAKE_C_FLAGS="-march=native"
make -j "$(nproc)" install
popd
rm -rf "$BUILD_DIR"
popd

echo "export H2O_HOME=$H2O_HOME" > "${IROOT}/h2o.installed"
echo -e "export PATH=\$H2O_HOME/bin:\$PATH" >> "${IROOT}/h2o.installed"

source "${IROOT}/h2o.installed"
