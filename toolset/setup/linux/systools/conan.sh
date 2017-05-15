#!/bin/bash

fw_depends python-dev

fw_installed conan && return 0

pip install --user conan

echo -e "export PATH=~/.local/bin:\$PATH" > $IROOT/conan.installed

source $IROOT/conan.installed
