#!/bin/bash

VERSION="1.0.4"
ELIXIR_HOME=$IROOT/elixir
RETCODE=$(fw_exists ${ELIXIR_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $ELIXIR_HOME.installed
  return 0; }

fw_depends erlang

fw_get https://codeload.github.com/elixir-lang/elixir/tar.gz/v$VERSION
fw_untar v$VERSION

(
	mv elixir-$VERSION elixir
	cd elixir
	make compile
)

echo -e "export PATH=${ELIXIR_HOME}/bin:\$PATH" > $ELIXIR_HOME.installed

source $ELIXIR_HOME.installed
