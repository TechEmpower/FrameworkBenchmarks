#!/bin/bash

RETCODE=$(fw_exists $IROOT/elixir.installed)
[ ! "$RETCODE" == 0 ] || { . $IROOT/elixir.installed; return 0; }

fw_depends erlang
export PATH=$PATH:$IROOT/erlang/bin

VERSION="1.0.4"
fw_get https://codeload.github.com/elixir-lang/elixir/tar.gz/v$VERSION
fw_untar v$VERSION

(
	mv elixir-$VERSION elixir
	cd elixir
	make compile
)

echo "export PATH=$IROOT/erlang/bin:$IROOT/elixir/bin:$PATH" >> $IROOT/elixir.installed
