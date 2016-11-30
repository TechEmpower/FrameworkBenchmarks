#!/bin/bash

fw_depends erlang

RETCODE=$(fw_exists ${IROOT}/elixir.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/elixir.installed
  return 0; }

ELIXIR_HOME=$IROOT/elixir
VERSION="1.3.3-1"
RELEASE="trusty"
ARCH="all"

fw_get -O http://packages.erlang-solutions.com/debian/pool/elixir_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb
dpkg -x elixir_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb $IROOT/elixir
$IROOT/erlang/usr/lib/erlang/Install -minimal $IROOT/erlang/usr/lib/erlang

echo "export ELIXIR_HOME=${ELIXIR_HOME}" > $IROOT/elixir.installed
echo -e "export PATH=\$ELIXIR_HOME/usr/local/bin:\$PATH" >> $IROOT/elixir.installed

source $IROOT/elixir.installed
