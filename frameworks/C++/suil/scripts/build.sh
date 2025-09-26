#!/bin/bash

set -e

# Build project
mkdir -p .build
cd .build
cmake .. -DCMAKE_BUILD_TYPE=Release \
         -DCMAKE_INSTALL_PREFIX=/install \
         -DSUIL_BASE_PATH=$(cat /var/SUIL_BASE)
make install
