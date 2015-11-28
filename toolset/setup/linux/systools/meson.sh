#!/bin/bash

fw_depends python3

fw_installed meson && return 0

pip3 install meson

touch $IROOT/meson.installed
