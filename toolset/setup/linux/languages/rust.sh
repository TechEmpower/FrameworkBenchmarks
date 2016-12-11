#!/bin/bash

RUST_VERSION="1.13.0"

RETCODE=$(fw_exists $IROOT/rust.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/rust.installed;
  return 0; }

fw_get -O https://static.rust-lang.org/dist/rust-${RUST_VERSION}-x86_64-unknown-linux-gnu.tar.gz
fw_untar rust-${RUST_VERSION}-x86_64-unknown-linux-gnu.tar.gz
(
	cd rust-${RUST_VERSION}-x86_64-unknown-linux-gnu
	./install.sh --prefix=$IROOT/rust
)

echo -e "export LD_LIBRARY_PATH=${IROOT}/rust/lib:\$LD_LIBRARY_PATH" > $IROOT/rust.installed
echo -e "export PATH=${IROOT}/rust/bin:\$PATH" >> $IROOT/rust.installed

source $IROOT/rust.installed
