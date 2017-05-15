#!/bin/bash

fw_depends gcc-4.9 conan

fw_installed luna && return 0

echo "" > $IROOT/luna.installed

source $IROOT/luna.installed
