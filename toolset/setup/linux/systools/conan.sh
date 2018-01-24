#!/bin/bash

fw_depends python-dev

fw_installed conan && return 0

# We're using conan 0.28.1 because, as of this writing, later versions of conan
# are incompatible with the luna framework, and luna is the only framework that
# uses conan.
pip install --user -Iv conan==0.28.1

echo -e "export PATH=~/.local/bin:\$PATH" > $IROOT/conan.installed

source $IROOT/conan.installed
