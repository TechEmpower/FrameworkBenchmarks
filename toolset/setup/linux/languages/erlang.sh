#!/bin/bash

fw_installed erlang && return 0

VERSION="18.2-1"
RELEASE="trusty"
ARCH="amd64"


fw_get -O http://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb
dpkg -x esl-erlang_${VERSION}~ubuntu~${RELEASE}_${ARCH}.deb $IROOT/erlang
$IROOT/erlang/usr/lib/erlang/Install -minimal $IROOT/erlang/usr/lib/erlang
fw_get https://s3.amazonaws.com/rebar3/rebar3 && mv rebar3 $IROOT/erlang/usr/bin/rebar && chmod +x $IROOT/erlang/usr/bin/rebar
echo -e "export PATH=${IROOT}/erlang/usr/bin:\$PATH" > $IROOT/erlang.installed

source $IROOT/erlang.installed
