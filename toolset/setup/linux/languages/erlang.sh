#!/bin/bash

fw_installed erlang && return 0

VERSION="18.2-2"
RELEASE="trusty"
ARCH="amd64"

fw_get -O http://packages.erlang-solutions.com/site/esl/esl-erlang/FLAVOUR_2_esl/esl-erlang_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb
dpkg -x esl-erlang_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb $IROOT/erlang
$IROOT/erlang/usr/lib/erlang/Install -minimal $IROOT/erlang/usr/lib/erlang

echo -e "export PATH=${IROOT}/erlang/usr/bin:\$PATH" > $IROOT/erlang.installed

source $IROOT/erlang.installed
