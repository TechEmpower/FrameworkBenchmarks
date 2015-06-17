#!/bin/bash

RETCODE=$(fw_exists $IROOT/elixir.installed)
[ ! "$RETCODE" == 0 ] || { . $IROOT/elixir.installed; return 0; }

fw_depends erlang
source $IROOT/erlang.installed

VERSION="1.0.4-1"
RELEASE="trusty"
ARCH="amd64"

fw_get -O http://packages.erlang-solutions.com/site/esl/elixir/FLAVOUR_2_download/elixir_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb
dpkg -x elixir_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb $IROOT/elixir
$IROOT/erlang/usr/lib/erlang/Install -minimal $IROOT/erlang/usr/lib/erlang

echo "export PATH=\$IROOT/elixir/usr/local/bin:\$IROOT/erlang/usr/bin:\$PATH" >> $IROOT/elixir.installed
