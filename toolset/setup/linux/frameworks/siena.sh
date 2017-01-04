#!/bin/bash

fw_depends play1

fw_installed siena && return 0

VERSION="2.0.6"
SIENNA=$IROOT/siena-$VERSION

yes | play install siena-2.0.6

echo "" > $IROOT/siena.installed

source $IROOT/siena.installed
