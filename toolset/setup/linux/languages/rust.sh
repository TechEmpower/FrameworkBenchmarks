#!/bin/bash

RETCODE=$(fw_exists $IROOT/rust.installed)
[ ! "$RETCODE" == 0 ] || { . $IROOT/rust.installed; return 0; }

fw_get -O https://static.rust-lang.org/dist/rust-1.0.0-x86_64-unknown-linux-gnu.tar.gz
fw_untar rust-1.0.0-x86_64-unknown-linux-gnu.tar.gz
(
	cd rust-1.0.0-x86_64-unknown-linux-gnu
	./install.sh --prefix=$IROOT/rust
)

echo -e "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$IROOT/rust/lib" > $IROOT/rust.installed
echo -e "export PATH=$IROOT/rust/bin:$PATH" >> $IROOT/rust.installed
