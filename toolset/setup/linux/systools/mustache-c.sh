#!/bin/bash

RETCODE=$(fw_exists "${IROOT}/mustache-c.installed")
[ ! "$RETCODE" == 0 ] || { \
  source "${IROOT}/mustache-c.installed"
  return 0; }

MUSTACHE_C_HOME="${IROOT}/mustache-c"
BUILD_DIR="${MUSTACHE_C_HOME}_build"

git clone 'https://github.com/x86-64/mustache-c.git' "$BUILD_DIR"
pushd "$BUILD_DIR"
git checkout c00262c9bb57b7871b1c68930e85c8f0e4c8c7d9
./configure --prefix="$MUSTACHE_C_HOME"
make -j "$(nproc)" install
popd
rm -rf "$BUILD_DIR"

echo "export MUSTACHE_C_HOME=$MUSTACHE_C_HOME" > "${IROOT}/mustache-c.installed"
echo -e "export LD_LIBRARY_PATH=\$MUSTACHE_C_HOME/lib:\$LD_LIBRARY_PATH" \
	>> "${IROOT}/mustache-c.installed"

source "${IROOT}/mustache-c.installed"
