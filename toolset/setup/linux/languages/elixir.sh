#!/bin/bash

VERSION="1.0.4"
ELIXIR_HOME=$IROOT/elixir
RETCODE=$(fw_exists ${ELIXIR_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $ELIXIR_HOME.installed
  return 0; }

fw_depends erlang

VERSION="1.0.4-1"
RELEASE="trusty"
ARCH="amd64"

fw_get -O http://packages.erlang-solutions.com/site/esl/elixir/FLAVOUR_2_download/elixir_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb
dpkg -x elixir_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb $IROOT/elixir
$IROOT/erlang/usr/lib/erlang/Install -minimal $IROOT/erlang/usr/lib/erlang

echo -e "export PATH=${ELIXIR_HOME}/usr/local/bin:\$PATH" > $ELIXIR_HOME.installed

source $ELIXIR_HOME.installed
