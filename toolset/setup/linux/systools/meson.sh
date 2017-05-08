#!/bin/bash

MESON_VERSION="0.40.1"

fw_depends python3

fw_installed meson && return 0

pip3 install meson==$MESON_VERSION

touch $IROOT/meson.installed
